# FASCL: per-polygon properties and lambdas in CTR

FASCL (FASt Calculation of Lambdas on Coupling Length) started as a Lisp program (2000-2001,
`/scratch1/skoranne/OLD_CDS/LATAKE/sandeep/DDL/fascl/src/fascl.lisp`): find externally facing
edges that meet a spacing criterion, then run a user condition and a user lambda on every
coupling pair and accumulate the result per net. This is the first stage of bringing it into
MAGPARSER: property tables on layers, and a CTR statement that runs a lambda once per polygon.
Coupling (two-polygon) lambdas over spacing-qualified edge pairs are the next stage.

## Property tables (`property_table.f90`)

Every `Layer` may carry a `PropertyTable` (`layer%props`), OASIS-like `NAME -> VALUE` per polygon.

* **Polygons** are the connected components of the layer's boxes, as numbered by the PNum pass
  (`PerformMerge` + `fullreduce`): multi-box polygons have labels `1..R`, single-rect polygons get
  `R+1..R+S` in box order, exactly as `uf%expand_roots` numbers them (the "Polygons"/"Rects" NSPT
  counts of the PNumLoop table). That number is the `id` a lambda receives. Boxes that touch at a
  corner belong to the same polygon.
* **Ordinal** (`POLYGON_NUMBER`) is the canonical index `1..N`: polygons sorted by their smallest box
  in `(x1,y1,x2,y2)` order, i.e. by left edge x, then the lowest y on that edge. It does not depend
  on box storage order, thread count or the CPU/GPU numbering path.
* **Value types**: `INT` (signed 64 bit), `FLOAT` (real64), `STRING`, `STRUCT` (ordered named
  fields, each INT/FLOAT/STRING; one level, no nesting). A column's type is decided when it is
  committed: STRING if any value is a string, else FLOAT if any value is a float, else INT; mixing
  strings and numbers, or STRUCT and scalars, is an error.
* **Validity**: columns remember the geometry signature (box count + order-independent box hash).
  Re-sorting or compacting a layer keeps its properties (the index is rebuilt); overwriting the layer
  drops them with a warning. `COPY` carries properties to the output layer.

## CTR statements

```
PROPERTY:PNUM = d1:met1 FASCL [ (id) PNUM = POLYGON_NUMBER(id) ] d1:nothing
exec run PROPERTY:A,B = d1:met1 FASCL [ (id) a = AREA(id); A = a / 1e6; B = IF(a > 1e6, "big", "small") ] nothing
PROPERTY:INFO = d1:met1 FASCL [ (id) INFO = STRUCT(w = BBOX_WIDTH(id), h = BBOX_HEIGHT(id)); INFO.tag = "m1" ] d1:nothing
PROPERTY:BIG = d1:met1 FASCL [ (id) WHEN AREA(id) > 1000000; BIG = 1 ] d1:nothing
PROPERTY:ROWY = d1:met1 FASCL [ (id) BBOX_Y1(id) ] d1:nothing
exec propinfo d1:met1                    # summary of the table
exec propdump d1:met1 met1_props.csv     # ordinal,id,nboxes,area,x1,y1,x2,y2,<properties>
exec oasout d1 out.oas                   # every non-empty layer of d1, with properties
exec oasout d1:met1 met1.oas [68 20]     # one layer (layer/datatype from the name or given)
```

* The names left of `=` (comma separated) are the properties the statement defines on the first
  operand layer; every one of them must be assigned in the lambda. Other assigned names are locals.
  A lambda that is a single expression defines the single property named on the left.
* The second operand is the coupling partner layer of the coming spacing stage; unary lambdas
  take `nothing` (or `db:nothing`). A non-empty second layer is rejected for now.
* Lines may be up to 4096 characters (1024 when TRS rewriting is enabled). CTR files go through `m4`: avoid lower-case m4 built-in names
  (`len`, `index`, `eval`, `substr`, ...) and the quote characters `` ` `` and `'` in lambdas.

### Lambda language

```
lambda := '[' '(' param {',' param} ')' stmt {';' stmt} ']'
stmt   := NAME '=' expr | NAME '.' FIELD '=' expr | 'WHEN' expr | expr
expr   := || && == != < <= > >= + - * / % unary - !, parentheses
          literals 123  1.5  2.0e-3  "text" (backslash escapes the next character)
          NAME (parameter or earlier assignment), NAME.field, f(args), STRUCT(f = expr, ...)
```

| Function | Result |
|---|---|
| `POLYGON_NUMBER(id)` | canonical ordinal 1..N |
| `POLYGON_ID(id)` | the PNum id itself |
| `AREA(id)` | area in dbu^2 (INT) |
| `NUM_BOXES(id)`, `NUM_RECTS(id)` | boxes in the polygon |
| `BBOX_X1/Y1/X2/Y2(id)`, `BBOX_WIDTH(id)`, `BBOX_HEIGHT(id)` | bounding box, dbu |
| `INT(x)`, `FLOAT(x)`, `STRING(x)` | conversions (STRING of a FLOAT uses 17 digits) |
| `ABS`, `SQRT`, `POW(a,b)`, `MIN(a,b)`, `MAX(a,b)`, `IF(c,a,b)` | arithmetic (`IF` evaluates one branch) |
| `NAME(id)`, `NAME.field(id)` | an existing property (or STRUCT field) of this layer |

`/` always gives FLOAT, `%` is integer modulo, `+` concatenates when either side is a STRING,
comparisons give INT 0/1. `WHEN c` drops the polygon (no value for any defined property) unless
`c` is true. Runtime errors (type mismatch, division by zero, missing property) stop the run with
the first failing polygon's ordinal and id. The loop over polygons runs in parallel (OpenMP).

## OASIS output (`prop_oasis.f90`, `db/prop_oasis_writer.cpp`)

`exec oasout`, and any output layer whose `.mag` entry names a `*.oas` file (e.g.
`KLBIN met1_L68_D20.oas`), write SEMI P39 OASIS with the properties of every polygon:

* each polygon is one element: `RECTANGLE` (with the square form when w == h) or a Manhattan
  `POLYGON` (point-list type 2) traced from the healed boxes; holes are joined to the outer
  contour by zero-width keyhole cuts, and corner-connected parts form one self-touching outline;
* after each element, one `PROPERTY` record per defined property, the name by `PROPNAME`
  reference: INT -> signed integer (type 9), FLOAT -> IEEE double (type 7), STRING -> `PROPSTRING`
  reference (type 13/14/15 by character class), STRUCT -> one property whose value list is
  `[field-name, value, ...]` (KLayout reads it as a list). A record identical to the previous one is
  written as `PROPERTY-REPEAT` (29);
* **pattern recognition**: polygons with the same shape *and the same property records* are
  grouped, and their positions decomposed into repetitions (type 1 lattices, 2/3 rows and columns,
  4/6 irregular rows/columns, 10 arbitrary). A PROPERTY after a repeated element applies to every
  instance, so instances that differ in any property value are never merged. Modal variables
  (layer, datatype, width, height, point list, repetition, XYRELATIVE positions) suppress repeated
  fields;
* the cell body is deflate-compressed into `CBLOCK`s that always end on a record boundary;
* **STRICT** mode: START offset-flag 1, contiguous explicit-reference `CELLNAME`, `PROPNAME`,
  `PROPSTRING` and `LAYERNAME` tables after the cell, strict table offsets in the 256-byte END.

Options (`var`): `oasis_strict`, `oasis_cblock`, `oasis_repetition` (0/1, default 1),
`oasis_cblock_bytes N` (uncompressed bytes per CBLOCK, default 4 MiB). Layer/datatype come from the
layer or file name (`L68_20`, `68:20`, `..._L68_D20...`), else the layer index and datatype 0.

## Regression

`db/regressions/fascl/run_fascl_regression.sh` builds a GDS with KLayout, runs the lambdas and
writes OASIS in every option combination, then checks with KLayout: geometry XOR, one shape per
connected polygon, every property value recomputed independently from the input, the CSV dumps,
STRICT table offsets, CBLOCKs, repetition types and expanded instance counts, PROPERTY-REPEAT, and
that a bad lambda reports a runtime error.
