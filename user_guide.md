# MAGVIEWER User Guide

`MAGVIEWER` (`MAGVIEWER.exe`) is an OpenGL/GLFW layout viewer for VLSI designs backed by a compressed spatial index (16-bit quantized R-Tree + BoxCodec/Zstd chunks + per-layer LRU cache).

## Supported Formats

| Format | Extensions |
| :--- | :--- |
| Magic | `.mag` (default for unrecognized extensions) |
| GDSII | `.gds`, `.gds2`, `.gdsii` |
| OASIS | `.oas`, `.oasis` |
| Single-layer binary / SNAP | `.bin`, `.snap`, `.snaz` |

## Command-Line Usage

```bash
MAGVIEWER.exe <layout_file> [max_layers] [--hier | -H] [--info | --headless]
```

- `<layout_file>`: Path to the input layout file.
- `[max_layers]`: Maximum number of layers to load (default: `256`).
- `--hier`, `-H`: Load GDSII/OASIS using the hierarchical compressed index (default: flat native compressed index).
- `--info`, `--headless`: Print layout/index memory statistics and run headless LOD and detail query benchmarks without opening a window.

## Window Layout

The default `1600 x 900` window is divided into three regions:

- **Main Viewport (right)**: Renders visible layers with filled quads and black outlines (capped at `500,000` quads per layer).
- **Layer Panel (top-left, `200 px` wide)**: 5-column grid of color-coded layer toggles, followed by **All OFF** (dark gray, white inner border) and **All ON** (light gray, white inner border) buttons.
- **Minimap (bottom-left)**: Shows the global layout bounding box with a yellow outline marking the current viewport.

## Keyboard Controls

| Key | Action |
| :--- | :--- |
| `Up` / `Down` / `Left` / `Right` | Pan the viewport (hold to pan continuously; speed scales with zoom) |
| `Z` or `=` | Zoom in |
| `X` or `-` | Zoom out (clamped to full-chip extent) |
| `Ctrl + A` | Reset view to fit the full layout bounding box (global MBR) |
| `R` | Toggle R-Tree LOD mode (`tree_mode`, starts ON) vs. exact box rendering |
| `T` | Increase R-Tree LOD depth (`target_depth + 1`, starts at `2`) |
| `S` | Decrease R-Tree LOD depth (`target_depth - 1`, minimum `0`) |
| `/` | Print world `(X, Y)` coordinates under the mouse cursor to stdout (main viewport only) |
| `Esc` | Quit `MAGVIEWER` |

## Mouse Controls

- **Left-click layer swatch** (top-left panel): Toggle that layer's visibility (bright = visible, dimmed = hidden) and highlight it as active.
- **Left-click All OFF / All ON** (last two outlined buttons in the layer panel): Hide or show all layers at once.
- **Hover main viewport + press `/`**: Inspect exact layout world coordinates in the terminal.

## Notes

- Toggling `R`, `S`, or `T` logs the updated `tree_mode` or `target_depth` value to stdout.
- Sub-pixel culling automatically filters geometry smaller than half a screen pixel (`min_span = 0.5 * zoom`) during interactive navigation.
