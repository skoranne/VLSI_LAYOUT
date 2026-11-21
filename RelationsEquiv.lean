/- RelationsEquiv.lean -/
namespace VLSI.RelationsEquiv
abbrev S := Bool
def sand (a b : S) : S := a && b
def sor  (a b : S) : S := a || b
def snot (a b : S) : S := a && !b
def sxor (a b : S) : S := xor a b
def sinside (a b : S) : S := a && b
def soutside (a b : S) : S := a && !b

theorem e2_0_0 (A B : S) : (sand (! A) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_1 (A B : S) : (sand (! B) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_2 (A B : S) : (sand A (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_3 (A B : S) : (sand B (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_4 (A B : S) : (sinside (! A) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_5 (A B : S) : (sinside (! B) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_6 (A B : S) : (sinside A (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_7 (A B : S) : (sinside B (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_8 (A B : S) : (snot (! A) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_9 (A B : S) : (sxor (! A) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_10 (A B : S) : (snot (! B) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_11 (A B : S) : (sxor (! B) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_12 (A B : S) : (soutside (! A) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_13 (A B : S) : (soutside (! B) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_14 (A B : S) : (snot A (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_15 (A B : S) : (snot A (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_16 (A B : S) : (snot B (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_17 (A B : S) : (snot B (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_18 (A B : S) : (sand (snot (! A) B) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_19 (A B : S) : (sand (snot (! A) B) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_20 (A B : S) : (sand (sand A (! A)) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_21 (A B : S) : (sand (sand A (! A)) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_22 (A B : S) : (snot (sand A (! A)) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_23 (A B : S) : (snot (sand A (! A)) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_24 (A B : S) : (sand (sand A (! B)) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_25 (A B : S) : (snot (sand A (! B)) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_26 (A B : S) : (snot (snot A (! B)) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_27 (A B : S) : (snot (snot A (! B)) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_28 (A B : S) : (sand (sand B (! A)) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_29 (A B : S) : (snot (sand B (! A)) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_30 (A B : S) : (sand A (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_31 (A B : S) : (sand A (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_32 (A B : S) : (sand A (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_33 (A B : S) : (sand B (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_34 (A B : S) : (sand B (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_35 (A B : S) : (sand B (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_36 (A B : S) : (sinside (snot (! A) B) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_37 (A B : S) : (sinside (snot (! A) B) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_38 (A B : S) : (sinside (sand A (! A)) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_39 (A B : S) : (sinside (sand A (! A)) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_40 (A B : S) : (sinside (sand A (! B)) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_41 (A B : S) : (sinside (sand B (! A)) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_42 (A B : S) : (sinside A (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_43 (A B : S) : (sinside A (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_44 (A B : S) : (sinside A (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_45 (A B : S) : (soutside A (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_46 (A B : S) : (soutside A (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_47 (A B : S) : (sinside B (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_48 (A B : S) : (sinside B (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_49 (A B : S) : (sinside B (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_50 (A B : S) : (soutside B (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_51 (A B : S) : (soutside B (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_52 (A B : S) : (soutside (sand A (! A)) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_53 (A B : S) : (soutside (sand A (! A)) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_54 (A B : S) : (soutside (sand A (! B)) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_55 (A B : S) : (soutside (snot A (! B)) A) = false := by cases A <;> cases B <;> rfl
theorem e2_0_56 (A B : S) : (soutside (snot A (! B)) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_57 (A B : S) : (soutside (sand B (! A)) B) = false := by cases A <;> cases B <;> rfl
theorem e2_0_58 (A B : S) : (snot (! A) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_59 (A B : S) : (snot (! A) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_60 (A B : S) : (snot (! B) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_61 (A B : S) : (snot (! B) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_62 (A B : S) : (snot (snot (! A) B) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_63 (A B : S) : (snot (snot (! A) B) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_64 (A B : S) : (sand (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_65 (A B : S) : (sand (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_66 (A B : S) : (snot (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_67 (A B : S) : (snot (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_68 (A B : S) : (sand (sand A (! B)) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_69 (A B : S) : (snot (sand A (! B)) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_70 (A B : S) : (sand (snot A (! B)) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_71 (A B : S) : (sand (snot A (! B)) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_72 (A B : S) : (sand (sand B (! A)) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_73 (A B : S) : (snot (sand B (! A)) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_74 (A B : S) : (sand (! A) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_75 (A B : S) : (sand (! A) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_76 (A B : S) : (sand (! A) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_77 (A B : S) : (sand (! B) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_78 (A B : S) : (sand (! B) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_79 (A B : S) : (sand (! B) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_80 (A B : S) : (sinside (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_81 (A B : S) : (sinside (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_82 (A B : S) : (sinside (sand A (! B)) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_83 (A B : S) : (sinside (snot A (! B)) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_84 (A B : S) : (sinside (snot A (! B)) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_85 (A B : S) : (sinside (sand B (! A)) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_86 (A B : S) : (sinside (! A) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_87 (A B : S) : (sinside (! A) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_88 (A B : S) : (sinside (! A) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_89 (A B : S) : (soutside (! A) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_90 (A B : S) : (soutside (! A) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_91 (A B : S) : (sinside (! B) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_92 (A B : S) : (sinside (! B) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_93 (A B : S) : (sinside (! B) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_94 (A B : S) : (soutside (! B) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_95 (A B : S) : (soutside (! B) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_96 (A B : S) : (soutside (snot (! A) B) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_97 (A B : S) : (soutside (snot (! A) B) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_98 (A B : S) : (soutside (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_99 (A B : S) : (soutside (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_100 (A B : S) : (soutside (sand A (! B)) (! B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_101 (A B : S) : (soutside (sand B (! A)) (! A)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_102 (A B : S) : (snot (! A) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_103 (A B : S) : (snot (! B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_104 (A B : S) : (snot (sor A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_105 (A B : S) : (sxor (sor A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_106 (A B : S) : (snot (sor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_107 (A B : S) : (snot (sor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_108 (A B : S) : (sxor (sor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_109 (A B : S) : (snot (sor B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_110 (A B : S) : (snot (sor B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_111 (A B : S) : (sxor (sor B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_112 (A B : S) : (snot (snot (! A) B) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_113 (A B : S) : (snot (snot (! A) B) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_114 (A B : S) : (snot (snot (! A) B) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_115 (A B : S) : (sand (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_116 (A B : S) : (sand (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_117 (A B : S) : (sand (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_118 (A B : S) : (snot (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_119 (A B : S) : (snot (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_120 (A B : S) : (snot (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_121 (A B : S) : (sor (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_122 (A B : S) : (sand (sand A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_123 (A B : S) : (snot (sand A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_124 (A B : S) : (snot (sand A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_125 (A B : S) : (snot (snot A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_126 (A B : S) : (snot (snot A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_127 (A B : S) : (snot (snot A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_128 (A B : S) : (sand (sor A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_129 (A B : S) : (sand (sor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_130 (A B : S) : (sand (sor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_131 (A B : S) : (snot (sxor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_132 (A B : S) : (snot (sxor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_133 (A B : S) : (snot (sxor A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_134 (A B : S) : (sand (sand B (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_135 (A B : S) : (snot (sand B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_136 (A B : S) : (snot (sand B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_137 (A B : S) : (sand (sor B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_138 (A B : S) : (sand (sor B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_139 (A B : S) : (sand (snot (! A) B) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_140 (A B : S) : (sand (snot (! A) B) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_141 (A B : S) : (sand (snot (! A) B) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_142 (A B : S) : (sand (snot (! A) B) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_143 (A B : S) : (snot (snot (! A) B) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_144 (A B : S) : (snot (snot (! A) B) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_145 (A B : S) : (sxor (snot (! A) B) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_146 (A B : S) : (sand (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_147 (A B : S) : (sand (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_148 (A B : S) : (sand (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_149 (A B : S) : (sand (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_150 (A B : S) : (sand (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_151 (A B : S) : (sand (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_152 (A B : S) : (snot (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_153 (A B : S) : (snot (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_154 (A B : S) : (snot (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_155 (A B : S) : (snot (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_156 (A B : S) : (snot (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_157 (A B : S) : (snot (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_158 (A B : S) : (sxor (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_159 (A B : S) : (sand (sand A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_160 (A B : S) : (sand (sand A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_161 (A B : S) : (sand (sand A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_162 (A B : S) : (sand (sand A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_163 (A B : S) : (sand (sand A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_164 (A B : S) : (snot (sand A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_165 (A B : S) : (sxor (sand A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_166 (A B : S) : (sand (snot A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_167 (A B : S) : (sand (snot A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_168 (A B : S) : (sand (snot A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_169 (A B : S) : (sand (snot A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_170 (A B : S) : (snot (snot A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_171 (A B : S) : (snot (snot A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_172 (A B : S) : (sxor (snot A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_173 (A B : S) : (sand (sxor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_174 (A B : S) : (sand (sxor A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_175 (A B : S) : (sand (sxor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_176 (A B : S) : (snot (sxor A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_177 (A B : S) : (sxor (sxor A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_178 (A B : S) : (sand (sand B (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_179 (A B : S) : (sand (sand B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_180 (A B : S) : (sand (sand B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_181 (A B : S) : (sand (sand B (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_182 (A B : S) : (sand (sand B (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_183 (A B : S) : (snot (sand B (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_184 (A B : S) : (sxor (sand B (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_185 (A B : S) : (soutside (! A) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_186 (A B : S) : (soutside (! B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_187 (A B : S) : (sinside (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_188 (A B : S) : (sinside (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_189 (A B : S) : (sinside (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_190 (A B : S) : (sinside (sand A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_191 (A B : S) : (sinside (sor A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_192 (A B : S) : (soutside (sor A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_193 (A B : S) : (sinside (sor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_194 (A B : S) : (sinside (sor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_195 (A B : S) : (soutside (sor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_196 (A B : S) : (soutside (sor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_197 (A B : S) : (sinside (sand B (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_198 (A B : S) : (sinside (sor B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_199 (A B : S) : (sinside (sor B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_200 (A B : S) : (soutside (sor B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_201 (A B : S) : (soutside (sor B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_202 (A B : S) : (sinside (snot (! A) B) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_203 (A B : S) : (sinside (snot (! A) B) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_204 (A B : S) : (sinside (snot (! A) B) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_205 (A B : S) : (sinside (snot (! A) B) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_206 (A B : S) : (soutside (snot (! A) B) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_207 (A B : S) : (soutside (snot (! A) B) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_208 (A B : S) : (soutside (snot (! A) B) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_209 (A B : S) : (sinside (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_210 (A B : S) : (sinside (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_211 (A B : S) : (sinside (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_212 (A B : S) : (sinside (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_213 (A B : S) : (sinside (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_214 (A B : S) : (sinside (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_215 (A B : S) : (soutside (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_216 (A B : S) : (soutside (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_217 (A B : S) : (soutside (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_218 (A B : S) : (sinside (sand A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_219 (A B : S) : (sinside (sand A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_220 (A B : S) : (sinside (sand A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_221 (A B : S) : (sinside (sand A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_222 (A B : S) : (sinside (sand A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_223 (A B : S) : (soutside (sand A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_224 (A B : S) : (soutside (sand A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_225 (A B : S) : (sinside (snot A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_226 (A B : S) : (sinside (snot A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_227 (A B : S) : (sinside (snot A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_228 (A B : S) : (sinside (snot A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_229 (A B : S) : (soutside (snot A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_230 (A B : S) : (soutside (snot A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_231 (A B : S) : (soutside (snot A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_232 (A B : S) : (sinside (sxor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_233 (A B : S) : (sinside (sxor A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_234 (A B : S) : (sinside (sxor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_235 (A B : S) : (soutside (sxor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_236 (A B : S) : (soutside (sxor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_237 (A B : S) : (soutside (sxor A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_238 (A B : S) : (sinside (sand B (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_239 (A B : S) : (sinside (sand B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_240 (A B : S) : (sinside (sand B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_241 (A B : S) : (sinside (sand B (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_242 (A B : S) : (sinside (sand B (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_243 (A B : S) : (soutside (sand B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_244 (A B : S) : (soutside (sand B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_245 (A B : S) : (soutside (snot (! A) B) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_246 (A B : S) : (soutside (snot (! A) B) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_247 (A B : S) : (snot (sor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_248 (A B : S) : (soutside (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_249 (A B : S) : (soutside (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_250 (A B : S) : (soutside (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_251 (A B : S) : (soutside (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_252 (A B : S) : (soutside (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_253 (A B : S) : (soutside (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_254 (A B : S) : (soutside (sand A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_255 (A B : S) : (soutside (snot A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_256 (A B : S) : (soutside (snot A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_257 (A B : S) : (soutside (sxor A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_258 (A B : S) : (soutside (sand B (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_259 (A B : S) : (snot (snot (! A) B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_260 (A B : S) : (sand (sor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_261 (A B : S) : (sand (sor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_262 (A B : S) : (snot (sxor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_263 (A B : S) : (sand (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_264 (A B : S) : (snot (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_265 (A B : S) : (snot (sand A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_266 (A B : S) : (sand (snot A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_267 (A B : S) : (snot (sand B (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_268 (A B : S) : (sand (snot (! A) B) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_269 (A B : S) : (sand (sxor (! A) (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_270 (A B : S) : (sand (sxor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_271 (A B : S) : (sand (sxor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_272 (A B : S) : (sand (sxor (! A) (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_273 (A B : S) : (sand (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_274 (A B : S) : (snot (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_275 (A B : S) : (snot (sand A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_276 (A B : S) : (sand (snot A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_277 (A B : S) : (sand (sxor A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_278 (A B : S) : (snot (sand B (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_279 (A B : S) : (sinside (sor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_280 (A B : S) : (sinside (sor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_281 (A B : S) : (soutside (sor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_282 (A B : S) : (sinside (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_283 (A B : S) : (sinside (snot A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_284 (A B : S) : (sinside (snot (! A) B) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_285 (A B : S) : (soutside (snot (! A) B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_286 (A B : S) : (sinside (sxor (! A) (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> rfl
theorem e2_0_287 (A B : S) : (sinside (sxor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_288 (A B : S) : (sinside (sxor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_289 (A B : S) : (sinside (sxor (! A) (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_290 (A B : S) : (soutside (sxor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_291 (A B : S) : (sinside (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_292 (A B : S) : (soutside (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_293 (A B : S) : (soutside (sand A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_294 (A B : S) : (sinside (snot A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_295 (A B : S) : (sinside (sxor A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_296 (A B : S) : (soutside (sand B (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_297 (A B : S) : (snot (sor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_298 (A B : S) : (sxor (sor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_299 (A B : S) : (soutside (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_300 (A B : S) : (soutside (sand A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_301 (A B : S) : (soutside (sand B (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_302 (A B : S) : (snot (sxor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_303 (A B : S) : (snot (sxor (! A) (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_304 (A B : S) : (sxor (sxor (! A) (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_305 (A B : S) : (soutside (sor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_306 (A B : S) : (soutside (sxor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_0_307 (A B : S) : (soutside (sxor (! A) (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> rfl
theorem e2_A_0 (A B : S) : (snot A (! A)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_1 (A B : S) : (soutside A (! A)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_2 (A B : S) : (sor (sand A (! A)) A) = A := by cases A <;> cases B <;> rfl
theorem e2_A_3 (A B : S) : (sor (sand A (! B)) A) = A := by cases A <;> cases B <;> rfl
theorem e2_A_4 (A B : S) : (sor (snot A (! B)) A) = A := by cases A <;> cases B <;> rfl
theorem e2_A_5 (A B : S) : (sand (sor A (! A)) A) = A := by cases A <;> cases B <;> rfl
theorem e2_A_6 (A B : S) : (sand (sor A (! B)) A) = A := by cases A <;> cases B <;> rfl
theorem e2_A_7 (A B : S) : (sand A (sor A (! A))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_8 (A B : S) : (sand A (sor A (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_9 (A B : S) : (sor A (sand A (! A))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_10 (A B : S) : (sor A (sand A (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_11 (A B : S) : (sor A (snot A (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_12 (A B : S) : (sxor (sand A (! A)) A) = A := by cases A <;> cases B <;> rfl
theorem e2_A_13 (A B : S) : (snot A (snot (! A) B)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_14 (A B : S) : (snot A (sand A (! A))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_15 (A B : S) : (snot A (sand B (! A))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_16 (A B : S) : (sxor A (sand A (! A))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_17 (A B : S) : (sinside (sor A (! A)) A) = A := by cases A <;> cases B <;> rfl
theorem e2_A_18 (A B : S) : (sinside (sor A (! B)) A) = A := by cases A <;> cases B <;> rfl
theorem e2_A_19 (A B : S) : (sinside A (sor A (! A))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_20 (A B : S) : (sinside A (sor A (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_21 (A B : S) : (soutside A (snot (! A) B)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_22 (A B : S) : (soutside A (sand A (! A))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_23 (A B : S) : (soutside A (sand B (! A))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_24 (A B : S) : (snot (sor A (! A)) (! A)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_25 (A B : S) : (sxor (sor A (! A)) (! A)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_26 (A B : S) : (snot (sor A (! B)) (! A)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_27 (A B : S) : (sxor (! A) (sor A (! A))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_28 (A B : S) : (sxor (sxor (! A) (! B)) B) = A := by cases A <;> cases B <;> rfl
theorem e2_A_29 (A B : S) : (sxor (sxor A (! B)) (! B)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_30 (A B : S) : (sxor (! B) (sxor A (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_31 (A B : S) : (sxor B (sxor (! A) (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_32 (A B : S) : (soutside (sor A (! A)) (! A)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_33 (A B : S) : (soutside (sor A (! B)) (! A)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_34 (A B : S) : (sxor (snot (! A) B) (sor A (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_35 (A B : S) : (sor (sand A (! B)) (snot A (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_36 (A B : S) : (sor (snot A (! B)) (sand A (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_37 (A B : S) : (snot (sor A (! B)) (snot (! A) B)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_38 (A B : S) : (sxor (sor A (! B)) (snot (! A) B)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_39 (A B : S) : (sxor (sand A (! B)) (snot A (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_40 (A B : S) : (sxor (snot A (! B)) (sand A (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_41 (A B : S) : (soutside (sor A (! B)) (snot (! A) B)) = A := by cases A <;> cases B <;> rfl
theorem e2_A_42 (A B : S) : (sxor (sor (! A) (! B)) (sor B (! A))) = A := by cases A <;> cases B <;> rfl
theorem e2_A_43 (A B : S) : (sxor (sor B (! A)) (sor (! A) (! B))) = A := by cases A <;> cases B <;> rfl
theorem e2_B_0 (A B : S) : (snot B (! B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_1 (A B : S) : (soutside B (! B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_2 (A B : S) : (sor (sand A (! A)) B) = B := by cases A <;> cases B <;> rfl
theorem e2_B_3 (A B : S) : (sor (snot A (! B)) B) = B := by cases A <;> cases B <;> rfl
theorem e2_B_4 (A B : S) : (sand (sor A (! A)) B) = B := by cases A <;> cases B <;> rfl
theorem e2_B_5 (A B : S) : (sor (sand B (! A)) B) = B := by cases A <;> cases B <;> rfl
theorem e2_B_6 (A B : S) : (sand (sor B (! A)) B) = B := by cases A <;> cases B <;> rfl
theorem e2_B_7 (A B : S) : (sand B (sor A (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_8 (A B : S) : (sand B (sor B (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_9 (A B : S) : (sor B (sand A (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_10 (A B : S) : (sor B (snot A (! B))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_11 (A B : S) : (sor B (sand B (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_12 (A B : S) : (sxor (sand A (! A)) B) = B := by cases A <;> cases B <;> rfl
theorem e2_B_13 (A B : S) : (snot B (snot (! A) B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_14 (A B : S) : (snot B (sand A (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_15 (A B : S) : (snot B (sand A (! B))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_16 (A B : S) : (sxor B (sand A (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_17 (A B : S) : (sinside (sor A (! A)) B) = B := by cases A <;> cases B <;> rfl
theorem e2_B_18 (A B : S) : (sinside (sor B (! A)) B) = B := by cases A <;> cases B <;> rfl
theorem e2_B_19 (A B : S) : (sinside B (sor A (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_20 (A B : S) : (sinside B (sor B (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_21 (A B : S) : (soutside B (snot (! A) B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_22 (A B : S) : (soutside B (sand A (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_23 (A B : S) : (soutside B (sand A (! B))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_24 (A B : S) : (snot (sor A (! A)) (! B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_25 (A B : S) : (sxor (sor A (! A)) (! B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_26 (A B : S) : (snot (sor B (! A)) (! B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_27 (A B : S) : (sxor (! B) (sor A (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_28 (A B : S) : (sxor (sxor (! A) (! B)) A) = B := by cases A <;> cases B <;> rfl
theorem e2_B_29 (A B : S) : (sxor (sxor A (! B)) (! A)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_30 (A B : S) : (sxor (! A) (sxor A (! B))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_31 (A B : S) : (sxor A (sxor (! A) (! B))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_32 (A B : S) : (soutside (sor A (! A)) (! B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_33 (A B : S) : (soutside (sor B (! A)) (! B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_34 (A B : S) : (sxor (snot (! A) B) (sor B (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_35 (A B : S) : (sor (snot A (! B)) (sand B (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_36 (A B : S) : (sor (sand B (! A)) (snot A (! B))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_37 (A B : S) : (snot (sor B (! A)) (snot (! A) B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_38 (A B : S) : (sxor (sor B (! A)) (snot (! A) B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_39 (A B : S) : (sxor (snot A (! B)) (sand B (! A))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_40 (A B : S) : (sxor (sand B (! A)) (snot A (! B))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_41 (A B : S) : (soutside (sor B (! A)) (snot (! A) B)) = B := by cases A <;> cases B <;> rfl
theorem e2_B_42 (A B : S) : (sxor (sor (! A) (! B)) (sor A (! B))) = B := by cases A <;> cases B <;> rfl
theorem e2_B_43 (A B : S) : (sxor (sor A (! B)) (sor (! A) (! B))) = B := by cases A <;> cases B <;> rfl

theorem e3_0_0 (A B C : S) : (sand (! A) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_1 (A B C : S) : (sand (! B) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_2 (A B C : S) : (sand (! C) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_3 (A B C : S) : (sand A (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_4 (A B C : S) : (sand B (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_5 (A B C : S) : (sand C (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_6 (A B C : S) : (sinside (! A) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_7 (A B C : S) : (sinside (! B) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_8 (A B C : S) : (sinside (! C) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_9 (A B C : S) : (sinside A (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_10 (A B C : S) : (sinside B (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_11 (A B C : S) : (sinside C (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_12 (A B C : S) : (snot (! A) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_13 (A B C : S) : (sxor (! A) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_14 (A B C : S) : (snot (! B) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_15 (A B C : S) : (sxor (! B) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_16 (A B C : S) : (snot (! C) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_17 (A B C : S) : (sxor (! C) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_18 (A B C : S) : (soutside (! A) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_19 (A B C : S) : (soutside (! B) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_20 (A B C : S) : (soutside (! C) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_21 (A B C : S) : (snot A (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_22 (A B C : S) : (snot A (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_23 (A B C : S) : (snot A (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_24 (A B C : S) : (snot B (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_25 (A B C : S) : (snot B (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_26 (A B C : S) : (snot B (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_27 (A B C : S) : (snot C (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_28 (A B C : S) : (snot C (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_29 (A B C : S) : (snot C (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_30 (A B C : S) : (sand (snot (! A) B) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_31 (A B C : S) : (sand (snot (! A) B) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_32 (A B C : S) : (sand (snot (! A) C) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_33 (A B C : S) : (sand (snot (! A) C) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_34 (A B C : S) : (sand (snot (! B) C) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_35 (A B C : S) : (sand (snot (! B) C) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_36 (A B C : S) : (sand (sand A (! A)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_37 (A B C : S) : (sand (sand A (! A)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_38 (A B C : S) : (sand (sand A (! A)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_39 (A B C : S) : (snot (sand A (! A)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_40 (A B C : S) : (snot (sand A (! A)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_41 (A B C : S) : (snot (sand A (! A)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_42 (A B C : S) : (sand (sand A (! B)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_43 (A B C : S) : (snot (sand A (! B)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_44 (A B C : S) : (sand (sand A (! C)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_45 (A B C : S) : (snot (sand A (! C)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_46 (A B C : S) : (snot (snot A (! B)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_47 (A B C : S) : (snot (snot A (! B)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_48 (A B C : S) : (snot (snot A (! C)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_49 (A B C : S) : (snot (snot A (! C)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_50 (A B C : S) : (sand (sand B (! A)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_51 (A B C : S) : (snot (sand B (! A)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_52 (A B C : S) : (sand (sand B (! C)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_53 (A B C : S) : (snot (sand B (! C)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_54 (A B C : S) : (snot (snot B (! C)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_55 (A B C : S) : (snot (snot B (! C)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_56 (A B C : S) : (sand (sand C (! A)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_57 (A B C : S) : (snot (sand C (! A)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_58 (A B C : S) : (sand (sand C (! B)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_59 (A B C : S) : (snot (sand C (! B)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_60 (A B C : S) : (sand A (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_61 (A B C : S) : (sand A (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_62 (A B C : S) : (sand A (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_63 (A B C : S) : (sand A (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_64 (A B C : S) : (sand A (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_65 (A B C : S) : (sand B (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_66 (A B C : S) : (sand B (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_67 (A B C : S) : (sand B (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_68 (A B C : S) : (sand B (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_69 (A B C : S) : (sand B (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_70 (A B C : S) : (sand C (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_71 (A B C : S) : (sand C (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_72 (A B C : S) : (sand C (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_73 (A B C : S) : (sand C (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_74 (A B C : S) : (sand C (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_75 (A B C : S) : (sinside (snot (! A) B) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_76 (A B C : S) : (sinside (snot (! A) B) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_77 (A B C : S) : (sinside (snot (! A) C) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_78 (A B C : S) : (sinside (snot (! A) C) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_79 (A B C : S) : (sinside (snot (! B) C) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_80 (A B C : S) : (sinside (snot (! B) C) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_81 (A B C : S) : (sinside (sand A (! A)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_82 (A B C : S) : (sinside (sand A (! A)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_83 (A B C : S) : (sinside (sand A (! A)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_84 (A B C : S) : (sinside (sand A (! B)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_85 (A B C : S) : (sinside (sand A (! C)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_86 (A B C : S) : (sinside (sand B (! A)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_87 (A B C : S) : (sinside (sand B (! C)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_88 (A B C : S) : (sinside (sand C (! A)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_89 (A B C : S) : (sinside (sand C (! B)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_90 (A B C : S) : (sinside A (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_91 (A B C : S) : (sinside A (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_92 (A B C : S) : (sinside A (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_93 (A B C : S) : (sinside A (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_94 (A B C : S) : (sinside A (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_95 (A B C : S) : (soutside A (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_96 (A B C : S) : (soutside A (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_97 (A B C : S) : (soutside A (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_98 (A B C : S) : (sinside B (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_99 (A B C : S) : (sinside B (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_100 (A B C : S) : (sinside B (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_101 (A B C : S) : (sinside B (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_102 (A B C : S) : (sinside B (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_103 (A B C : S) : (soutside B (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_104 (A B C : S) : (soutside B (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_105 (A B C : S) : (soutside B (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_106 (A B C : S) : (sinside C (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_107 (A B C : S) : (sinside C (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_108 (A B C : S) : (sinside C (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_109 (A B C : S) : (sinside C (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_110 (A B C : S) : (sinside C (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_111 (A B C : S) : (soutside C (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_112 (A B C : S) : (soutside C (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_113 (A B C : S) : (soutside C (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_114 (A B C : S) : (soutside (sand A (! A)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_115 (A B C : S) : (soutside (sand A (! A)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_116 (A B C : S) : (soutside (sand A (! A)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_117 (A B C : S) : (soutside (sand A (! B)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_118 (A B C : S) : (soutside (sand A (! C)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_119 (A B C : S) : (soutside (snot A (! B)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_120 (A B C : S) : (soutside (snot A (! B)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_121 (A B C : S) : (soutside (snot A (! C)) A) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_122 (A B C : S) : (soutside (snot A (! C)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_123 (A B C : S) : (soutside (sand B (! A)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_124 (A B C : S) : (soutside (sand B (! C)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_125 (A B C : S) : (soutside (snot B (! C)) B) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_126 (A B C : S) : (soutside (snot B (! C)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_127 (A B C : S) : (soutside (sand C (! A)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_128 (A B C : S) : (soutside (sand C (! B)) C) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_129 (A B C : S) : (snot (! A) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_130 (A B C : S) : (snot (! A) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_131 (A B C : S) : (snot (! A) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_132 (A B C : S) : (snot (! B) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_133 (A B C : S) : (snot (! B) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_134 (A B C : S) : (snot (! B) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_135 (A B C : S) : (snot (! C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_136 (A B C : S) : (snot (! C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_137 (A B C : S) : (snot (! C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_138 (A B C : S) : (snot (snot (! A) B) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_139 (A B C : S) : (snot (snot (! A) B) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_140 (A B C : S) : (snot (snot (! A) C) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_141 (A B C : S) : (snot (snot (! A) C) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_142 (A B C : S) : (snot (snot (! B) C) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_143 (A B C : S) : (snot (snot (! B) C) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_144 (A B C : S) : (sand (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_145 (A B C : S) : (sand (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_146 (A B C : S) : (sand (sand A (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_147 (A B C : S) : (snot (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_148 (A B C : S) : (snot (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_149 (A B C : S) : (snot (sand A (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_150 (A B C : S) : (sand (sand A (! B)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_151 (A B C : S) : (snot (sand A (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_152 (A B C : S) : (sand (sand A (! C)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_153 (A B C : S) : (snot (sand A (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_154 (A B C : S) : (sand (snot A (! B)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_155 (A B C : S) : (sand (snot A (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_156 (A B C : S) : (sand (snot A (! C)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_157 (A B C : S) : (sand (snot A (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_158 (A B C : S) : (sand (sand B (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_159 (A B C : S) : (snot (sand B (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_160 (A B C : S) : (sand (sand B (! C)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_161 (A B C : S) : (snot (sand B (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_162 (A B C : S) : (sand (snot B (! C)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_163 (A B C : S) : (sand (snot B (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_164 (A B C : S) : (sand (sand C (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_165 (A B C : S) : (snot (sand C (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_166 (A B C : S) : (sand (sand C (! B)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_167 (A B C : S) : (snot (sand C (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_168 (A B C : S) : (sand (! A) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_169 (A B C : S) : (sand (! A) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_170 (A B C : S) : (sand (! A) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_171 (A B C : S) : (sand (! A) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_172 (A B C : S) : (sand (! A) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_173 (A B C : S) : (sand (! B) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_174 (A B C : S) : (sand (! B) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_175 (A B C : S) : (sand (! B) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_176 (A B C : S) : (sand (! B) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_177 (A B C : S) : (sand (! B) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_178 (A B C : S) : (sand (! C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_179 (A B C : S) : (sand (! C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_180 (A B C : S) : (sand (! C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_181 (A B C : S) : (sand (! C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_182 (A B C : S) : (sand (! C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_183 (A B C : S) : (sinside (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_184 (A B C : S) : (sinside (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_185 (A B C : S) : (sinside (sand A (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_186 (A B C : S) : (sinside (sand A (! B)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_187 (A B C : S) : (sinside (sand A (! C)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_188 (A B C : S) : (sinside (snot A (! B)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_189 (A B C : S) : (sinside (snot A (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_190 (A B C : S) : (sinside (snot A (! C)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_191 (A B C : S) : (sinside (snot A (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_192 (A B C : S) : (sinside (sand B (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_193 (A B C : S) : (sinside (sand B (! C)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_194 (A B C : S) : (sinside (snot B (! C)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_195 (A B C : S) : (sinside (snot B (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_196 (A B C : S) : (sinside (sand C (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_197 (A B C : S) : (sinside (sand C (! B)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_198 (A B C : S) : (sinside (! A) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_199 (A B C : S) : (sinside (! A) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_200 (A B C : S) : (sinside (! A) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_201 (A B C : S) : (sinside (! A) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_202 (A B C : S) : (sinside (! A) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_203 (A B C : S) : (soutside (! A) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_204 (A B C : S) : (soutside (! A) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_205 (A B C : S) : (soutside (! A) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_206 (A B C : S) : (sinside (! B) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_207 (A B C : S) : (sinside (! B) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_208 (A B C : S) : (sinside (! B) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_209 (A B C : S) : (sinside (! B) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_210 (A B C : S) : (sinside (! B) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_211 (A B C : S) : (soutside (! B) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_212 (A B C : S) : (soutside (! B) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_213 (A B C : S) : (soutside (! B) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_214 (A B C : S) : (sinside (! C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_215 (A B C : S) : (sinside (! C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_216 (A B C : S) : (sinside (! C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_217 (A B C : S) : (sinside (! C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_218 (A B C : S) : (sinside (! C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_219 (A B C : S) : (soutside (! C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_220 (A B C : S) : (soutside (! C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_221 (A B C : S) : (soutside (! C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_222 (A B C : S) : (soutside (snot (! A) B) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_223 (A B C : S) : (soutside (snot (! A) B) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_224 (A B C : S) : (soutside (snot (! A) C) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_225 (A B C : S) : (soutside (snot (! A) C) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_226 (A B C : S) : (soutside (snot (! B) C) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_227 (A B C : S) : (soutside (snot (! B) C) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_228 (A B C : S) : (soutside (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_229 (A B C : S) : (soutside (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_230 (A B C : S) : (soutside (sand A (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_231 (A B C : S) : (soutside (sand A (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_232 (A B C : S) : (soutside (sand A (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_233 (A B C : S) : (soutside (sand B (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_234 (A B C : S) : (soutside (sand B (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_235 (A B C : S) : (soutside (sand C (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_236 (A B C : S) : (soutside (sand C (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_237 (A B C : S) : (snot (! A) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_238 (A B C : S) : (snot (! A) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_239 (A B C : S) : (snot (! B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_240 (A B C : S) : (snot (! B) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_241 (A B C : S) : (snot (! C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_242 (A B C : S) : (snot (! C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_243 (A B C : S) : (snot (sor A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_244 (A B C : S) : (sxor (sor A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_245 (A B C : S) : (snot (sor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_246 (A B C : S) : (snot (sor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_247 (A B C : S) : (sxor (sor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_248 (A B C : S) : (snot (sor A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_249 (A B C : S) : (snot (sor A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_250 (A B C : S) : (sxor (sor A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_251 (A B C : S) : (snot (sor B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_252 (A B C : S) : (snot (sor B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_253 (A B C : S) : (sxor (sor B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_254 (A B C : S) : (snot (sor B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_255 (A B C : S) : (snot (sor B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_256 (A B C : S) : (sxor (sor B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_257 (A B C : S) : (snot (sor C (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_258 (A B C : S) : (snot (sor C (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_259 (A B C : S) : (sxor (sor C (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_260 (A B C : S) : (snot (sor C (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_261 (A B C : S) : (snot (sor C (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_262 (A B C : S) : (sxor (sor C (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_263 (A B C : S) : (snot (snot (! A) B) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_264 (A B C : S) : (snot (snot (! A) B) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_265 (A B C : S) : (snot (snot (! A) B) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_266 (A B C : S) : (snot (snot (! A) B) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_267 (A B C : S) : (snot (snot (! A) B) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_268 (A B C : S) : (snot (snot (! A) C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_269 (A B C : S) : (snot (snot (! A) C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_270 (A B C : S) : (snot (snot (! A) C) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_271 (A B C : S) : (snot (snot (! A) C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_272 (A B C : S) : (snot (snot (! A) C) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_273 (A B C : S) : (snot (snot (! B) C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_274 (A B C : S) : (snot (snot (! B) C) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_275 (A B C : S) : (snot (snot (! B) C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_276 (A B C : S) : (snot (snot (! B) C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_277 (A B C : S) : (snot (snot (! B) C) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_278 (A B C : S) : (sand (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_279 (A B C : S) : (sand (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_280 (A B C : S) : (sand (sand A (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_281 (A B C : S) : (sand (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_282 (A B C : S) : (sand (sand A (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_283 (A B C : S) : (sand (sand A (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_284 (A B C : S) : (sand (sand A (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_285 (A B C : S) : (snot (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_286 (A B C : S) : (snot (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_287 (A B C : S) : (snot (sand A (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_288 (A B C : S) : (snot (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_289 (A B C : S) : (snot (sand A (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_290 (A B C : S) : (snot (sand A (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_291 (A B C : S) : (snot (sand A (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_292 (A B C : S) : (sor (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_293 (A B C : S) : (sand (sand A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_294 (A B C : S) : (snot (sand A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_295 (A B C : S) : (snot (sand A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_296 (A B C : S) : (snot (sand A (! B)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_297 (A B C : S) : (snot (sand A (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_298 (A B C : S) : (sand (sand A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_299 (A B C : S) : (snot (sand A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_300 (A B C : S) : (snot (sand A (! C)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_301 (A B C : S) : (snot (sand A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_302 (A B C : S) : (snot (sand A (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_303 (A B C : S) : (snot (snot A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_304 (A B C : S) : (snot (snot A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_305 (A B C : S) : (snot (snot A (! B)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_306 (A B C : S) : (snot (snot A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_307 (A B C : S) : (snot (snot A (! B)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_308 (A B C : S) : (snot (snot A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_309 (A B C : S) : (snot (snot A (! C)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_310 (A B C : S) : (snot (snot A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_311 (A B C : S) : (snot (snot A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_312 (A B C : S) : (snot (snot A (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_313 (A B C : S) : (sand (sor A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_314 (A B C : S) : (sand (sor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_315 (A B C : S) : (sand (sor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_316 (A B C : S) : (sand (sor A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_317 (A B C : S) : (sand (sor A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_318 (A B C : S) : (snot (sxor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_319 (A B C : S) : (snot (sxor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_320 (A B C : S) : (snot (sxor A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_321 (A B C : S) : (snot (sxor A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_322 (A B C : S) : (snot (sxor A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_323 (A B C : S) : (snot (sxor A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_324 (A B C : S) : (sand (sand B (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_325 (A B C : S) : (snot (sand B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_326 (A B C : S) : (snot (sand B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_327 (A B C : S) : (snot (sand B (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_328 (A B C : S) : (snot (sand B (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_329 (A B C : S) : (sand (sand B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_330 (A B C : S) : (snot (sand B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_331 (A B C : S) : (snot (sand B (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_332 (A B C : S) : (snot (sand B (! C)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_333 (A B C : S) : (snot (sand B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_334 (A B C : S) : (snot (snot B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_335 (A B C : S) : (snot (snot B (! C)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_336 (A B C : S) : (snot (snot B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_337 (A B C : S) : (snot (snot B (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_338 (A B C : S) : (snot (snot B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_339 (A B C : S) : (sand (sor B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_340 (A B C : S) : (sand (sor B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_341 (A B C : S) : (sand (sor B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_342 (A B C : S) : (sand (sor B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_343 (A B C : S) : (snot (sxor B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_344 (A B C : S) : (snot (sxor B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_345 (A B C : S) : (snot (sxor B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_346 (A B C : S) : (sand (sand C (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_347 (A B C : S) : (snot (sand C (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_348 (A B C : S) : (snot (sand C (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_349 (A B C : S) : (snot (sand C (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_350 (A B C : S) : (snot (sand C (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_351 (A B C : S) : (sand (sand C (! B)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_352 (A B C : S) : (snot (sand C (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_353 (A B C : S) : (snot (sand C (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_354 (A B C : S) : (snot (sand C (! B)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_355 (A B C : S) : (snot (sand C (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_356 (A B C : S) : (sand (sor C (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_357 (A B C : S) : (sand (sor C (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_358 (A B C : S) : (sand (sor C (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_359 (A B C : S) : (sand (sor C (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_360 (A B C : S) : (sand (snot (! A) B) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_361 (A B C : S) : (sand (snot (! A) B) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_362 (A B C : S) : (sand (snot (! A) B) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_363 (A B C : S) : (sand (snot (! A) B) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_364 (A B C : S) : (sand (snot (! A) B) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_365 (A B C : S) : (sand (snot (! A) B) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_366 (A B C : S) : (sand (snot (! A) B) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_367 (A B C : S) : (sand (snot (! A) B) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_368 (A B C : S) : (snot (snot (! A) B) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_369 (A B C : S) : (snot (snot (! A) B) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_370 (A B C : S) : (sxor (snot (! A) B) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_371 (A B C : S) : (sand (snot (! A) C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_372 (A B C : S) : (sand (snot (! A) C) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_373 (A B C : S) : (sand (snot (! A) C) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_374 (A B C : S) : (sand (snot (! A) C) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_375 (A B C : S) : (sand (snot (! A) C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_376 (A B C : S) : (sand (snot (! A) C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_377 (A B C : S) : (sand (snot (! A) C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_378 (A B C : S) : (sand (snot (! A) C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_379 (A B C : S) : (snot (snot (! A) C) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_380 (A B C : S) : (snot (snot (! A) C) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_381 (A B C : S) : (sxor (snot (! A) C) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_382 (A B C : S) : (sand (snot (! B) C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_383 (A B C : S) : (sand (snot (! B) C) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_384 (A B C : S) : (sand (snot (! B) C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_385 (A B C : S) : (sand (snot (! B) C) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_386 (A B C : S) : (sand (snot (! B) C) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_387 (A B C : S) : (sand (snot (! B) C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_388 (A B C : S) : (sand (snot (! B) C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_389 (A B C : S) : (sand (snot (! B) C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_390 (A B C : S) : (snot (snot (! B) C) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_391 (A B C : S) : (snot (snot (! B) C) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_392 (A B C : S) : (sxor (snot (! B) C) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_393 (A B C : S) : (sand (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_394 (A B C : S) : (sand (sand A (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_395 (A B C : S) : (sand (sand A (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_396 (A B C : S) : (sand (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_397 (A B C : S) : (sand (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_398 (A B C : S) : (sand (sand A (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_399 (A B C : S) : (sand (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_400 (A B C : S) : (sand (sand A (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_401 (A B C : S) : (sand (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_402 (A B C : S) : (sand (sand A (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_403 (A B C : S) : (sand (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_404 (A B C : S) : (sand (sand A (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_405 (A B C : S) : (sand (sand A (! A)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_406 (A B C : S) : (sand (sand A (! A)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_407 (A B C : S) : (sand (sand A (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_408 (A B C : S) : (sand (sand A (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_409 (A B C : S) : (snot (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_410 (A B C : S) : (snot (sand A (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_411 (A B C : S) : (snot (sand A (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_412 (A B C : S) : (snot (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_413 (A B C : S) : (snot (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_414 (A B C : S) : (snot (sand A (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_415 (A B C : S) : (snot (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_416 (A B C : S) : (snot (sand A (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_417 (A B C : S) : (snot (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_418 (A B C : S) : (snot (sand A (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_419 (A B C : S) : (snot (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_420 (A B C : S) : (snot (sand A (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_421 (A B C : S) : (snot (sand A (! A)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_422 (A B C : S) : (snot (sand A (! A)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_423 (A B C : S) : (snot (sand A (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_424 (A B C : S) : (snot (sand A (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_425 (A B C : S) : (sxor (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_426 (A B C : S) : (sand (sand A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_427 (A B C : S) : (sand (sand A (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_428 (A B C : S) : (sand (sand A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_429 (A B C : S) : (sand (sand A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_430 (A B C : S) : (sand (sand A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_431 (A B C : S) : (sand (sand A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_432 (A B C : S) : (sand (sand A (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_433 (A B C : S) : (sand (sand A (! B)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_434 (A B C : S) : (sand (sand A (! B)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_435 (A B C : S) : (snot (sand A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_436 (A B C : S) : (sxor (sand A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_437 (A B C : S) : (sand (sand A (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_438 (A B C : S) : (sand (sand A (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_439 (A B C : S) : (sand (sand A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_440 (A B C : S) : (sand (sand A (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_441 (A B C : S) : (sand (sand A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_442 (A B C : S) : (sand (sand A (! C)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_443 (A B C : S) : (sand (sand A (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_444 (A B C : S) : (sand (sand A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_445 (A B C : S) : (sand (sand A (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_446 (A B C : S) : (snot (sand A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_447 (A B C : S) : (sxor (sand A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_448 (A B C : S) : (sand (snot A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_449 (A B C : S) : (sand (snot A (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_450 (A B C : S) : (sand (snot A (! B)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_451 (A B C : S) : (sand (snot A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_452 (A B C : S) : (sand (snot A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_453 (A B C : S) : (sand (snot A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_454 (A B C : S) : (sand (snot A (! B)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_455 (A B C : S) : (sand (snot A (! B)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_456 (A B C : S) : (snot (snot A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_457 (A B C : S) : (snot (snot A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_458 (A B C : S) : (sxor (snot A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_459 (A B C : S) : (sand (snot A (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_460 (A B C : S) : (sand (snot A (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_461 (A B C : S) : (sand (snot A (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_462 (A B C : S) : (sand (snot A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_463 (A B C : S) : (sand (snot A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_464 (A B C : S) : (sand (snot A (! C)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_465 (A B C : S) : (sand (snot A (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_466 (A B C : S) : (sand (snot A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_467 (A B C : S) : (snot (snot A (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_468 (A B C : S) : (snot (snot A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_469 (A B C : S) : (sxor (snot A (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_470 (A B C : S) : (sand (sxor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_471 (A B C : S) : (sand (sxor A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_472 (A B C : S) : (sand (sxor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_473 (A B C : S) : (snot (sxor A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_474 (A B C : S) : (sxor (sxor A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_475 (A B C : S) : (sand (sxor A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_476 (A B C : S) : (sand (sxor A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_477 (A B C : S) : (sand (sxor A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_478 (A B C : S) : (snot (sxor A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_479 (A B C : S) : (sxor (sxor A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_480 (A B C : S) : (sand (sand B (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_481 (A B C : S) : (sand (sand B (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_482 (A B C : S) : (sand (sand B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_483 (A B C : S) : (sand (sand B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_484 (A B C : S) : (sand (sand B (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_485 (A B C : S) : (sand (sand B (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_486 (A B C : S) : (sand (sand B (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_487 (A B C : S) : (sand (sand B (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_488 (A B C : S) : (sand (sand B (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_489 (A B C : S) : (snot (sand B (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_490 (A B C : S) : (sxor (sand B (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_491 (A B C : S) : (sand (sand B (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_492 (A B C : S) : (sand (sand B (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_493 (A B C : S) : (sand (sand B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_494 (A B C : S) : (sand (sand B (! C)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_495 (A B C : S) : (sand (sand B (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_496 (A B C : S) : (sand (sand B (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_497 (A B C : S) : (sand (sand B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_498 (A B C : S) : (sand (sand B (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_499 (A B C : S) : (sand (sand B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_500 (A B C : S) : (snot (sand B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_501 (A B C : S) : (sxor (sand B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_502 (A B C : S) : (sand (snot B (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_503 (A B C : S) : (sand (snot B (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_504 (A B C : S) : (sand (snot B (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_505 (A B C : S) : (sand (snot B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_506 (A B C : S) : (sand (snot B (! C)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_507 (A B C : S) : (sand (snot B (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_508 (A B C : S) : (sand (snot B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_509 (A B C : S) : (sand (snot B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_510 (A B C : S) : (snot (snot B (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_511 (A B C : S) : (snot (snot B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_512 (A B C : S) : (sxor (snot B (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_513 (A B C : S) : (sand (sxor B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_514 (A B C : S) : (sand (sxor B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_515 (A B C : S) : (sand (sxor B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_516 (A B C : S) : (snot (sxor B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_517 (A B C : S) : (sxor (sxor B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_518 (A B C : S) : (sand (sand C (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_519 (A B C : S) : (sand (sand C (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_520 (A B C : S) : (sand (sand C (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_521 (A B C : S) : (sand (sand C (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_522 (A B C : S) : (sand (sand C (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_523 (A B C : S) : (sand (sand C (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_524 (A B C : S) : (sand (sand C (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_525 (A B C : S) : (sand (sand C (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_526 (A B C : S) : (sand (sand C (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_527 (A B C : S) : (snot (sand C (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_528 (A B C : S) : (sxor (sand C (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_529 (A B C : S) : (sand (sand C (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_530 (A B C : S) : (sand (sand C (! B)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_531 (A B C : S) : (sand (sand C (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_532 (A B C : S) : (sand (sand C (! B)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_533 (A B C : S) : (sand (sand C (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_534 (A B C : S) : (sand (sand C (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_535 (A B C : S) : (sand (sand C (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_536 (A B C : S) : (sand (sand C (! B)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_537 (A B C : S) : (sand (sand C (! B)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_538 (A B C : S) : (snot (sand C (! B)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_539 (A B C : S) : (sxor (sand C (! B)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_540 (A B C : S) : (soutside (! A) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_541 (A B C : S) : (soutside (! A) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_542 (A B C : S) : (soutside (! B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_543 (A B C : S) : (soutside (! B) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_544 (A B C : S) : (soutside (! C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_545 (A B C : S) : (soutside (! C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_546 (A B C : S) : (sinside (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_547 (A B C : S) : (sinside (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_548 (A B C : S) : (sinside (sand A (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_549 (A B C : S) : (sinside (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_550 (A B C : S) : (sinside (sand A (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_551 (A B C : S) : (sinside (sand A (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_552 (A B C : S) : (sinside (sand A (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_553 (A B C : S) : (sinside (sand A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_554 (A B C : S) : (sinside (sand A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_555 (A B C : S) : (sinside (sor A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_556 (A B C : S) : (soutside (sor A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_557 (A B C : S) : (sinside (sor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_558 (A B C : S) : (sinside (sor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_559 (A B C : S) : (soutside (sor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_560 (A B C : S) : (soutside (sor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_561 (A B C : S) : (sinside (sor A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_562 (A B C : S) : (sinside (sor A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_563 (A B C : S) : (soutside (sor A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_564 (A B C : S) : (soutside (sor A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_565 (A B C : S) : (sinside (sand B (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_566 (A B C : S) : (sinside (sand B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_567 (A B C : S) : (sinside (sor B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_568 (A B C : S) : (sinside (sor B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_569 (A B C : S) : (soutside (sor B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_570 (A B C : S) : (soutside (sor B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_571 (A B C : S) : (sinside (sor B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_572 (A B C : S) : (sinside (sor B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_573 (A B C : S) : (soutside (sor B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_574 (A B C : S) : (soutside (sor B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_575 (A B C : S) : (sinside (sand C (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_576 (A B C : S) : (sinside (sand C (! B)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_577 (A B C : S) : (sinside (sor C (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_578 (A B C : S) : (sinside (sor C (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_579 (A B C : S) : (soutside (sor C (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_580 (A B C : S) : (soutside (sor C (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_581 (A B C : S) : (sinside (sor C (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_582 (A B C : S) : (sinside (sor C (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_583 (A B C : S) : (soutside (sor C (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_584 (A B C : S) : (soutside (sor C (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_585 (A B C : S) : (sinside (snot (! A) B) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_586 (A B C : S) : (sinside (snot (! A) B) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_587 (A B C : S) : (sinside (snot (! A) B) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_588 (A B C : S) : (sinside (snot (! A) B) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_589 (A B C : S) : (sinside (snot (! A) B) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_590 (A B C : S) : (sinside (snot (! A) B) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_591 (A B C : S) : (sinside (snot (! A) B) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_592 (A B C : S) : (sinside (snot (! A) B) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_593 (A B C : S) : (soutside (snot (! A) B) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_594 (A B C : S) : (soutside (snot (! A) B) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_595 (A B C : S) : (soutside (snot (! A) B) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_596 (A B C : S) : (soutside (snot (! A) B) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_597 (A B C : S) : (soutside (snot (! A) B) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_598 (A B C : S) : (sinside (snot (! A) C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_599 (A B C : S) : (sinside (snot (! A) C) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_600 (A B C : S) : (sinside (snot (! A) C) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_601 (A B C : S) : (sinside (snot (! A) C) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_602 (A B C : S) : (sinside (snot (! A) C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_603 (A B C : S) : (sinside (snot (! A) C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_604 (A B C : S) : (sinside (snot (! A) C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_605 (A B C : S) : (sinside (snot (! A) C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_606 (A B C : S) : (soutside (snot (! A) C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_607 (A B C : S) : (soutside (snot (! A) C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_608 (A B C : S) : (soutside (snot (! A) C) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_609 (A B C : S) : (soutside (snot (! A) C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_610 (A B C : S) : (soutside (snot (! A) C) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_611 (A B C : S) : (sinside (snot (! B) C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_612 (A B C : S) : (sinside (snot (! B) C) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_613 (A B C : S) : (sinside (snot (! B) C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_614 (A B C : S) : (sinside (snot (! B) C) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_615 (A B C : S) : (sinside (snot (! B) C) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_616 (A B C : S) : (sinside (snot (! B) C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_617 (A B C : S) : (sinside (snot (! B) C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_618 (A B C : S) : (sinside (snot (! B) C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_619 (A B C : S) : (soutside (snot (! B) C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_620 (A B C : S) : (soutside (snot (! B) C) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_621 (A B C : S) : (soutside (snot (! B) C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_622 (A B C : S) : (soutside (snot (! B) C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_623 (A B C : S) : (soutside (snot (! B) C) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_624 (A B C : S) : (sinside (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_625 (A B C : S) : (sinside (sand A (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_626 (A B C : S) : (sinside (sand A (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_627 (A B C : S) : (sinside (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_628 (A B C : S) : (sinside (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_629 (A B C : S) : (sinside (sand A (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_630 (A B C : S) : (sinside (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_631 (A B C : S) : (sinside (sand A (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_632 (A B C : S) : (sinside (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_633 (A B C : S) : (sinside (sand A (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_634 (A B C : S) : (sinside (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_635 (A B C : S) : (sinside (sand A (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_636 (A B C : S) : (sinside (sand A (! A)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_637 (A B C : S) : (sinside (sand A (! A)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_638 (A B C : S) : (sinside (sand A (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_639 (A B C : S) : (sinside (sand A (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_640 (A B C : S) : (soutside (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_641 (A B C : S) : (soutside (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_642 (A B C : S) : (soutside (sand A (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_643 (A B C : S) : (soutside (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_644 (A B C : S) : (soutside (sand A (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_645 (A B C : S) : (soutside (sand A (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_646 (A B C : S) : (soutside (sand A (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_647 (A B C : S) : (sinside (sand A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_648 (A B C : S) : (sinside (sand A (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_649 (A B C : S) : (sinside (sand A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_650 (A B C : S) : (sinside (sand A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_651 (A B C : S) : (sinside (sand A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_652 (A B C : S) : (sinside (sand A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_653 (A B C : S) : (sinside (sand A (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_654 (A B C : S) : (sinside (sand A (! B)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_655 (A B C : S) : (sinside (sand A (! B)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_656 (A B C : S) : (soutside (sand A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_657 (A B C : S) : (soutside (sand A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_658 (A B C : S) : (soutside (sand A (! B)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_659 (A B C : S) : (soutside (sand A (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_660 (A B C : S) : (sinside (sand A (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_661 (A B C : S) : (sinside (sand A (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_662 (A B C : S) : (sinside (sand A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_663 (A B C : S) : (sinside (sand A (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_664 (A B C : S) : (sinside (sand A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_665 (A B C : S) : (sinside (sand A (! C)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_666 (A B C : S) : (sinside (sand A (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_667 (A B C : S) : (sinside (sand A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_668 (A B C : S) : (sinside (sand A (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_669 (A B C : S) : (soutside (sand A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_670 (A B C : S) : (soutside (sand A (! C)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_671 (A B C : S) : (soutside (sand A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_672 (A B C : S) : (soutside (sand A (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_673 (A B C : S) : (sinside (snot A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_674 (A B C : S) : (sinside (snot A (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_675 (A B C : S) : (sinside (snot A (! B)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_676 (A B C : S) : (sinside (snot A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_677 (A B C : S) : (sinside (snot A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_678 (A B C : S) : (sinside (snot A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_679 (A B C : S) : (sinside (snot A (! B)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_680 (A B C : S) : (sinside (snot A (! B)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_681 (A B C : S) : (soutside (snot A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_682 (A B C : S) : (soutside (snot A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_683 (A B C : S) : (soutside (snot A (! B)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_684 (A B C : S) : (soutside (snot A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_685 (A B C : S) : (soutside (snot A (! B)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_686 (A B C : S) : (sinside (snot A (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_687 (A B C : S) : (sinside (snot A (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_688 (A B C : S) : (sinside (snot A (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_689 (A B C : S) : (sinside (snot A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_690 (A B C : S) : (sinside (snot A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_691 (A B C : S) : (sinside (snot A (! C)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_692 (A B C : S) : (sinside (snot A (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_693 (A B C : S) : (sinside (snot A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_694 (A B C : S) : (soutside (snot A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_695 (A B C : S) : (soutside (snot A (! C)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_696 (A B C : S) : (soutside (snot A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_697 (A B C : S) : (soutside (snot A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_698 (A B C : S) : (soutside (snot A (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_699 (A B C : S) : (sinside (sxor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_700 (A B C : S) : (sinside (sxor A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_701 (A B C : S) : (sinside (sxor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_702 (A B C : S) : (soutside (sxor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_703 (A B C : S) : (soutside (sxor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_704 (A B C : S) : (soutside (sxor A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_705 (A B C : S) : (sinside (sxor A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_706 (A B C : S) : (sinside (sxor A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_707 (A B C : S) : (sinside (sxor A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_708 (A B C : S) : (soutside (sxor A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_709 (A B C : S) : (soutside (sxor A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_710 (A B C : S) : (soutside (sxor A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_711 (A B C : S) : (sinside (sand B (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_712 (A B C : S) : (sinside (sand B (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_713 (A B C : S) : (sinside (sand B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_714 (A B C : S) : (sinside (sand B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_715 (A B C : S) : (sinside (sand B (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_716 (A B C : S) : (sinside (sand B (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_717 (A B C : S) : (sinside (sand B (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_718 (A B C : S) : (sinside (sand B (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_719 (A B C : S) : (sinside (sand B (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_720 (A B C : S) : (soutside (sand B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_721 (A B C : S) : (soutside (sand B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_722 (A B C : S) : (soutside (sand B (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_723 (A B C : S) : (soutside (sand B (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_724 (A B C : S) : (sinside (sand B (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_725 (A B C : S) : (sinside (sand B (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_726 (A B C : S) : (sinside (sand B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_727 (A B C : S) : (sinside (sand B (! C)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_728 (A B C : S) : (sinside (sand B (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_729 (A B C : S) : (sinside (sand B (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_730 (A B C : S) : (sinside (sand B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_731 (A B C : S) : (sinside (sand B (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_732 (A B C : S) : (sinside (sand B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_733 (A B C : S) : (soutside (sand B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_734 (A B C : S) : (soutside (sand B (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_735 (A B C : S) : (soutside (sand B (! C)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_736 (A B C : S) : (soutside (sand B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_737 (A B C : S) : (sinside (snot B (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_738 (A B C : S) : (sinside (snot B (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_739 (A B C : S) : (sinside (snot B (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_740 (A B C : S) : (sinside (snot B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_741 (A B C : S) : (sinside (snot B (! C)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_742 (A B C : S) : (sinside (snot B (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_743 (A B C : S) : (sinside (snot B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_744 (A B C : S) : (sinside (snot B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_745 (A B C : S) : (soutside (snot B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_746 (A B C : S) : (soutside (snot B (! C)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_747 (A B C : S) : (soutside (snot B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_748 (A B C : S) : (soutside (snot B (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_749 (A B C : S) : (soutside (snot B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_750 (A B C : S) : (sinside (sxor B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_751 (A B C : S) : (sinside (sxor B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_752 (A B C : S) : (sinside (sxor B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_753 (A B C : S) : (soutside (sxor B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_754 (A B C : S) : (soutside (sxor B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_755 (A B C : S) : (soutside (sxor B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_756 (A B C : S) : (sinside (sand C (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_757 (A B C : S) : (sinside (sand C (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_758 (A B C : S) : (sinside (sand C (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_759 (A B C : S) : (sinside (sand C (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_760 (A B C : S) : (sinside (sand C (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_761 (A B C : S) : (sinside (sand C (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_762 (A B C : S) : (sinside (sand C (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_763 (A B C : S) : (sinside (sand C (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_764 (A B C : S) : (sinside (sand C (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_765 (A B C : S) : (soutside (sand C (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_766 (A B C : S) : (soutside (sand C (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_767 (A B C : S) : (soutside (sand C (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_768 (A B C : S) : (soutside (sand C (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_769 (A B C : S) : (sinside (sand C (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_770 (A B C : S) : (sinside (sand C (! B)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_771 (A B C : S) : (sinside (sand C (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_772 (A B C : S) : (sinside (sand C (! B)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_773 (A B C : S) : (sinside (sand C (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_774 (A B C : S) : (sinside (sand C (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_775 (A B C : S) : (sinside (sand C (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_776 (A B C : S) : (sinside (sand C (! B)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_777 (A B C : S) : (sinside (sand C (! B)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_778 (A B C : S) : (soutside (sand C (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_779 (A B C : S) : (soutside (sand C (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_780 (A B C : S) : (soutside (sand C (! B)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_781 (A B C : S) : (soutside (sand C (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_782 (A B C : S) : (soutside (snot (! A) B) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_783 (A B C : S) : (soutside (snot (! A) B) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_784 (A B C : S) : (soutside (snot (! A) C) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_785 (A B C : S) : (soutside (snot (! A) C) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_786 (A B C : S) : (snot (sor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_787 (A B C : S) : (snot (sor (! A) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_788 (A B C : S) : (soutside (snot (! B) C) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_789 (A B C : S) : (soutside (snot (! B) C) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_790 (A B C : S) : (snot (sor (! B) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_791 (A B C : S) : (soutside (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_792 (A B C : S) : (soutside (sand A (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_793 (A B C : S) : (soutside (sand A (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_794 (A B C : S) : (soutside (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_795 (A B C : S) : (soutside (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_796 (A B C : S) : (soutside (sand A (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_797 (A B C : S) : (soutside (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_798 (A B C : S) : (soutside (sand A (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_799 (A B C : S) : (soutside (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_800 (A B C : S) : (soutside (sand A (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_801 (A B C : S) : (soutside (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_802 (A B C : S) : (soutside (sand A (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_803 (A B C : S) : (soutside (sand A (! A)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_804 (A B C : S) : (soutside (sand A (! A)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_805 (A B C : S) : (soutside (sand A (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_806 (A B C : S) : (soutside (sand A (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_807 (A B C : S) : (soutside (sand A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_808 (A B C : S) : (soutside (sand A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_809 (A B C : S) : (soutside (snot A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_810 (A B C : S) : (soutside (snot A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_811 (A B C : S) : (soutside (snot A (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_812 (A B C : S) : (soutside (snot A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_813 (A B C : S) : (soutside (sxor A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_814 (A B C : S) : (soutside (sxor A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_815 (A B C : S) : (soutside (sand B (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_816 (A B C : S) : (soutside (sand B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_817 (A B C : S) : (soutside (snot B (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_818 (A B C : S) : (soutside (snot B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_819 (A B C : S) : (soutside (sxor B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_820 (A B C : S) : (soutside (sand C (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_821 (A B C : S) : (soutside (sand C (! B)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_822 (A B C : S) : (snot (snot (! A) B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_823 (A B C : S) : (snot (snot (! A) B) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_824 (A B C : S) : (snot (snot (! A) B) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_825 (A B C : S) : (snot (snot (! A) C) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_826 (A B C : S) : (snot (snot (! A) C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_827 (A B C : S) : (snot (snot (! A) C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_828 (A B C : S) : (sand (sor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_829 (A B C : S) : (sand (sor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_830 (A B C : S) : (sand (sor (! A) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_831 (A B C : S) : (sand (sor (! A) (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_832 (A B C : S) : (snot (sxor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_833 (A B C : S) : (snot (sxor (! A) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_834 (A B C : S) : (snot (snot (! B) C) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_835 (A B C : S) : (snot (snot (! B) C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_836 (A B C : S) : (snot (snot (! B) C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_837 (A B C : S) : (sand (sor (! B) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_838 (A B C : S) : (sand (sor (! B) (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_839 (A B C : S) : (snot (sxor (! B) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_840 (A B C : S) : (sand (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_841 (A B C : S) : (sand (sand A (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_842 (A B C : S) : (sand (sand A (! A)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_843 (A B C : S) : (snot (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_844 (A B C : S) : (snot (sand A (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_845 (A B C : S) : (snot (sand A (! A)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_846 (A B C : S) : (snot (sand A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_847 (A B C : S) : (snot (sand A (! B)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_848 (A B C : S) : (snot (sand A (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_849 (A B C : S) : (snot (sand A (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_850 (A B C : S) : (sand (snot A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_851 (A B C : S) : (sand (snot A (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_852 (A B C : S) : (snot (sand B (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_853 (A B C : S) : (snot (sand B (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_854 (A B C : S) : (snot (sand B (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_855 (A B C : S) : (snot (sand B (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_856 (A B C : S) : (sand (snot B (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_857 (A B C : S) : (snot (sand C (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_858 (A B C : S) : (snot (sand C (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_859 (A B C : S) : (snot (sand C (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_860 (A B C : S) : (snot (sand C (! B)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_861 (A B C : S) : (sand (snot (! A) B) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_862 (A B C : S) : (sand (snot (! A) C) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_863 (A B C : S) : (sand (sxor (! A) (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_864 (A B C : S) : (sand (sxor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_865 (A B C : S) : (sand (sxor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_866 (A B C : S) : (sand (sxor (! A) (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_867 (A B C : S) : (sand (sxor (! A) (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_868 (A B C : S) : (sand (sxor (! A) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_869 (A B C : S) : (sand (sxor (! A) (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_870 (A B C : S) : (sand (sxor (! A) (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_871 (A B C : S) : (sand (snot (! B) C) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_872 (A B C : S) : (sand (sxor (! B) (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_873 (A B C : S) : (sand (sxor (! B) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_874 (A B C : S) : (sand (sxor (! B) (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_875 (A B C : S) : (sand (sxor (! B) (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_876 (A B C : S) : (sand (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_877 (A B C : S) : (sand (sand A (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_878 (A B C : S) : (sand (sand A (! A)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_879 (A B C : S) : (snot (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_880 (A B C : S) : (snot (sand A (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_881 (A B C : S) : (snot (sand A (! A)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_882 (A B C : S) : (snot (sand A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_883 (A B C : S) : (snot (sand A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_884 (A B C : S) : (sand (snot A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_885 (A B C : S) : (sand (snot A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_886 (A B C : S) : (sand (sxor A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_887 (A B C : S) : (sand (sxor A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_888 (A B C : S) : (snot (sand B (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_889 (A B C : S) : (snot (sand B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_890 (A B C : S) : (sand (snot B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_891 (A B C : S) : (sand (sxor B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_892 (A B C : S) : (snot (sand C (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_893 (A B C : S) : (snot (sand C (! B)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_894 (A B C : S) : (sinside (sor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_895 (A B C : S) : (sinside (sor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_896 (A B C : S) : (soutside (sor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_897 (A B C : S) : (sinside (sor (! A) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_898 (A B C : S) : (sinside (sor (! A) (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_899 (A B C : S) : (soutside (sor (! A) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_900 (A B C : S) : (sinside (sor (! B) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_901 (A B C : S) : (sinside (sor (! B) (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_902 (A B C : S) : (soutside (sor (! B) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_903 (A B C : S) : (sinside (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_904 (A B C : S) : (sinside (sand A (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_905 (A B C : S) : (sinside (sand A (! A)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_906 (A B C : S) : (sinside (snot A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_907 (A B C : S) : (sinside (snot A (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_908 (A B C : S) : (sinside (snot B (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_909 (A B C : S) : (sinside (snot (! A) B) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_910 (A B C : S) : (soutside (snot (! A) B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_911 (A B C : S) : (soutside (snot (! A) B) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_912 (A B C : S) : (soutside (snot (! A) B) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_913 (A B C : S) : (sinside (snot (! A) C) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_914 (A B C : S) : (soutside (snot (! A) C) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_915 (A B C : S) : (soutside (snot (! A) C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_916 (A B C : S) : (soutside (snot (! A) C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_917 (A B C : S) : (sinside (sxor (! A) (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_918 (A B C : S) : (sinside (sxor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_919 (A B C : S) : (sinside (sxor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_920 (A B C : S) : (sinside (sxor (! A) (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_921 (A B C : S) : (soutside (sxor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_922 (A B C : S) : (sinside (sxor (! A) (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_923 (A B C : S) : (sinside (sxor (! A) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_924 (A B C : S) : (sinside (sxor (! A) (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_925 (A B C : S) : (sinside (sxor (! A) (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_926 (A B C : S) : (soutside (sxor (! A) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_927 (A B C : S) : (sinside (snot (! B) C) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_928 (A B C : S) : (soutside (snot (! B) C) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_929 (A B C : S) : (soutside (snot (! B) C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_930 (A B C : S) : (soutside (snot (! B) C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_931 (A B C : S) : (sinside (sxor (! B) (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_932 (A B C : S) : (sinside (sxor (! B) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_933 (A B C : S) : (sinside (sxor (! B) (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_934 (A B C : S) : (sinside (sxor (! B) (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_935 (A B C : S) : (soutside (sxor (! B) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_936 (A B C : S) : (sinside (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_937 (A B C : S) : (sinside (sand A (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_938 (A B C : S) : (sinside (sand A (! A)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_939 (A B C : S) : (soutside (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_940 (A B C : S) : (soutside (sand A (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_941 (A B C : S) : (soutside (sand A (! A)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_942 (A B C : S) : (soutside (sand A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_943 (A B C : S) : (soutside (sand A (! B)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_944 (A B C : S) : (soutside (sand A (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_945 (A B C : S) : (soutside (sand A (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_946 (A B C : S) : (sinside (snot A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_947 (A B C : S) : (sinside (snot A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_948 (A B C : S) : (sinside (sxor A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_949 (A B C : S) : (sinside (sxor A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_950 (A B C : S) : (soutside (sand B (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_951 (A B C : S) : (soutside (sand B (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_952 (A B C : S) : (soutside (sand B (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_953 (A B C : S) : (soutside (sand B (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_954 (A B C : S) : (sinside (snot B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_955 (A B C : S) : (sinside (sxor B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_956 (A B C : S) : (soutside (sand C (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_957 (A B C : S) : (soutside (sand C (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_958 (A B C : S) : (soutside (sand C (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_959 (A B C : S) : (soutside (sand C (! B)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_960 (A B C : S) : (snot (sor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_961 (A B C : S) : (sxor (sor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_962 (A B C : S) : (snot (sor (! A) (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_963 (A B C : S) : (sxor (sor (! A) (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_964 (A B C : S) : (snot (sor (! B) (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_965 (A B C : S) : (sxor (sor (! B) (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_966 (A B C : S) : (soutside (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_967 (A B C : S) : (soutside (sand A (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_968 (A B C : S) : (soutside (sand A (! A)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_969 (A B C : S) : (soutside (sand A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_970 (A B C : S) : (soutside (sand A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_971 (A B C : S) : (soutside (sand B (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_972 (A B C : S) : (soutside (sand B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_973 (A B C : S) : (soutside (sand C (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_974 (A B C : S) : (soutside (sand C (! B)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_975 (A B C : S) : (snot (sxor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_976 (A B C : S) : (snot (sxor (! A) (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_977 (A B C : S) : (snot (sxor (! B) (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_978 (A B C : S) : (snot (sxor (! A) (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_979 (A B C : S) : (sxor (sxor (! A) (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_980 (A B C : S) : (snot (sxor (! A) (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_981 (A B C : S) : (sxor (sxor (! A) (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_982 (A B C : S) : (snot (sxor (! B) (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_983 (A B C : S) : (sxor (sxor (! B) (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_984 (A B C : S) : (soutside (sor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_985 (A B C : S) : (soutside (sor (! A) (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_986 (A B C : S) : (soutside (sor (! B) (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_987 (A B C : S) : (soutside (sxor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_988 (A B C : S) : (soutside (sxor (! A) (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_989 (A B C : S) : (soutside (sxor (! B) (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_990 (A B C : S) : (soutside (sxor (! A) (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_991 (A B C : S) : (soutside (sxor (! A) (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_0_992 (A B C : S) : (soutside (sxor (! B) (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_0 (A B C : S) : (snot A (! A)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_1 (A B C : S) : (soutside A (! A)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_2 (A B C : S) : (sor (sand A (! A)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_3 (A B C : S) : (sor (sand A (! B)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_4 (A B C : S) : (sor (sand A (! C)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_5 (A B C : S) : (sor (snot A (! B)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_6 (A B C : S) : (sor (snot A (! C)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_7 (A B C : S) : (sand (sor A (! A)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_8 (A B C : S) : (sand (sor A (! B)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_9 (A B C : S) : (sand (sor A (! C)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_10 (A B C : S) : (sand A (sor A (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_11 (A B C : S) : (sand A (sor A (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_12 (A B C : S) : (sand A (sor A (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_13 (A B C : S) : (sor A (sand A (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_14 (A B C : S) : (sor A (sand A (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_15 (A B C : S) : (sor A (sand A (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_16 (A B C : S) : (sor A (snot A (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_17 (A B C : S) : (sor A (snot A (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_18 (A B C : S) : (sxor (sand A (! A)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_19 (A B C : S) : (snot A (snot (! A) B)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_20 (A B C : S) : (snot A (snot (! A) C)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_21 (A B C : S) : (snot A (sand A (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_22 (A B C : S) : (snot A (sand B (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_23 (A B C : S) : (snot A (sand C (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_24 (A B C : S) : (sxor A (sand A (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_25 (A B C : S) : (sinside (sor A (! A)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_26 (A B C : S) : (sinside (sor A (! B)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_27 (A B C : S) : (sinside (sor A (! C)) A) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_28 (A B C : S) : (sinside A (sor A (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_29 (A B C : S) : (sinside A (sor A (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_30 (A B C : S) : (sinside A (sor A (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_31 (A B C : S) : (soutside A (snot (! A) B)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_32 (A B C : S) : (soutside A (snot (! A) C)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_33 (A B C : S) : (soutside A (sand A (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_34 (A B C : S) : (soutside A (sand B (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_35 (A B C : S) : (soutside A (sand C (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_36 (A B C : S) : (snot (sor A (! A)) (! A)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_37 (A B C : S) : (sxor (sor A (! A)) (! A)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_38 (A B C : S) : (snot (sor A (! B)) (! A)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_39 (A B C : S) : (snot (sor A (! C)) (! A)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_40 (A B C : S) : (sxor (! A) (sor A (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_41 (A B C : S) : (sxor (sxor (! A) (! B)) B) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_42 (A B C : S) : (sxor (sxor (! A) (! C)) C) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_43 (A B C : S) : (sxor (sxor A (! B)) (! B)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_44 (A B C : S) : (sxor (sxor A (! C)) (! C)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_45 (A B C : S) : (sxor (! B) (sxor A (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_46 (A B C : S) : (sxor (! C) (sxor A (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_47 (A B C : S) : (sxor B (sxor (! A) (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_48 (A B C : S) : (sxor C (sxor (! A) (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_49 (A B C : S) : (soutside (sor A (! A)) (! A)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_50 (A B C : S) : (soutside (sor A (! B)) (! A)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_51 (A B C : S) : (soutside (sor A (! C)) (! A)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_52 (A B C : S) : (sxor (snot (! A) B) (sor A (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_53 (A B C : S) : (sxor (snot (! A) C) (sor A (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_54 (A B C : S) : (sor (sand A (! B)) (snot A (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_55 (A B C : S) : (sor (sand A (! C)) (snot A (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_56 (A B C : S) : (sor (snot A (! B)) (sand A (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_57 (A B C : S) : (sor (snot A (! C)) (sand A (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_58 (A B C : S) : (snot (sor A (! B)) (snot (! A) B)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_59 (A B C : S) : (sxor (sor A (! B)) (snot (! A) B)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_60 (A B C : S) : (snot (sor A (! C)) (snot (! A) C)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_61 (A B C : S) : (sxor (sor A (! C)) (snot (! A) C)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_62 (A B C : S) : (sxor (sand A (! B)) (snot A (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_63 (A B C : S) : (sxor (sand A (! C)) (snot A (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_64 (A B C : S) : (sxor (snot A (! B)) (sand A (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_65 (A B C : S) : (sxor (snot A (! C)) (sand A (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_66 (A B C : S) : (soutside (sor A (! B)) (snot (! A) B)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_67 (A B C : S) : (soutside (sor A (! C)) (snot (! A) C)) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_68 (A B C : S) : (sxor (sor (! A) (! B)) (sor B (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_69 (A B C : S) : (sxor (sor (! A) (! C)) (sor C (! A))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_70 (A B C : S) : (sxor (sor B (! A)) (sor (! A) (! B))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_A_71 (A B C : S) : (sxor (sor C (! A)) (sor (! A) (! C))) = A := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_0 (A B C : S) : (snot B (! B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_1 (A B C : S) : (soutside B (! B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_2 (A B C : S) : (sor (sand A (! A)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_3 (A B C : S) : (sor (snot A (! B)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_4 (A B C : S) : (sand (sor A (! A)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_5 (A B C : S) : (sor (sand B (! A)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_6 (A B C : S) : (sor (sand B (! C)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_7 (A B C : S) : (sor (snot B (! C)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_8 (A B C : S) : (sand (sor B (! A)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_9 (A B C : S) : (sand (sor B (! C)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_10 (A B C : S) : (sand B (sor A (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_11 (A B C : S) : (sand B (sor B (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_12 (A B C : S) : (sand B (sor B (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_13 (A B C : S) : (sor B (sand A (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_14 (A B C : S) : (sor B (snot A (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_15 (A B C : S) : (sor B (sand B (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_16 (A B C : S) : (sor B (sand B (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_17 (A B C : S) : (sor B (snot B (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_18 (A B C : S) : (sxor (sand A (! A)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_19 (A B C : S) : (snot B (snot (! A) B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_20 (A B C : S) : (snot B (snot (! B) C)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_21 (A B C : S) : (snot B (sand A (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_22 (A B C : S) : (snot B (sand A (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_23 (A B C : S) : (snot B (sand C (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_24 (A B C : S) : (sxor B (sand A (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_25 (A B C : S) : (sinside (sor A (! A)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_26 (A B C : S) : (sinside (sor B (! A)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_27 (A B C : S) : (sinside (sor B (! C)) B) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_28 (A B C : S) : (sinside B (sor A (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_29 (A B C : S) : (sinside B (sor B (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_30 (A B C : S) : (sinside B (sor B (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_31 (A B C : S) : (soutside B (snot (! A) B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_32 (A B C : S) : (soutside B (snot (! B) C)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_33 (A B C : S) : (soutside B (sand A (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_34 (A B C : S) : (soutside B (sand A (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_35 (A B C : S) : (soutside B (sand C (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_36 (A B C : S) : (snot (sor A (! A)) (! B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_37 (A B C : S) : (sxor (sor A (! A)) (! B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_38 (A B C : S) : (snot (sor B (! A)) (! B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_39 (A B C : S) : (snot (sor B (! C)) (! B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_40 (A B C : S) : (sxor (! B) (sor A (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_41 (A B C : S) : (sxor (sxor (! A) (! B)) A) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_42 (A B C : S) : (sxor (sxor (! B) (! C)) C) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_43 (A B C : S) : (sxor (sxor A (! B)) (! A)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_44 (A B C : S) : (sxor (sxor B (! C)) (! C)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_45 (A B C : S) : (sxor (! A) (sxor A (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_46 (A B C : S) : (sxor (! C) (sxor B (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_47 (A B C : S) : (sxor A (sxor (! A) (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_48 (A B C : S) : (sxor C (sxor (! B) (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_49 (A B C : S) : (soutside (sor A (! A)) (! B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_50 (A B C : S) : (soutside (sor B (! A)) (! B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_51 (A B C : S) : (soutside (sor B (! C)) (! B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_52 (A B C : S) : (sxor (snot (! A) B) (sor B (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_53 (A B C : S) : (sxor (snot (! B) C) (sor B (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_54 (A B C : S) : (sor (snot A (! B)) (sand B (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_55 (A B C : S) : (sor (sand B (! A)) (snot A (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_56 (A B C : S) : (sor (sand B (! C)) (snot B (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_57 (A B C : S) : (sor (snot B (! C)) (sand B (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_58 (A B C : S) : (snot (sor B (! A)) (snot (! A) B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_59 (A B C : S) : (sxor (sor B (! A)) (snot (! A) B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_60 (A B C : S) : (snot (sor B (! C)) (snot (! B) C)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_61 (A B C : S) : (sxor (sor B (! C)) (snot (! B) C)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_62 (A B C : S) : (sxor (snot A (! B)) (sand B (! A))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_63 (A B C : S) : (sxor (sand B (! A)) (snot A (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_64 (A B C : S) : (sxor (sand B (! C)) (snot B (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_65 (A B C : S) : (sxor (snot B (! C)) (sand B (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_66 (A B C : S) : (soutside (sor B (! A)) (snot (! A) B)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_67 (A B C : S) : (soutside (sor B (! C)) (snot (! B) C)) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_68 (A B C : S) : (sxor (sor (! A) (! B)) (sor A (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_69 (A B C : S) : (sxor (sor (! B) (! C)) (sor C (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_70 (A B C : S) : (sxor (sor A (! B)) (sor (! A) (! B))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_B_71 (A B C : S) : (sxor (sor C (! B)) (sor (! B) (! C))) = B := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_0 (A B C : S) : (snot C (! C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_1 (A B C : S) : (soutside C (! C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_2 (A B C : S) : (sor (sand A (! A)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_3 (A B C : S) : (sor (snot A (! C)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_4 (A B C : S) : (sand (sor A (! A)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_5 (A B C : S) : (sor (snot B (! C)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_6 (A B C : S) : (sor (sand C (! A)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_7 (A B C : S) : (sor (sand C (! B)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_8 (A B C : S) : (sand (sor C (! A)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_9 (A B C : S) : (sand (sor C (! B)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_10 (A B C : S) : (sand C (sor A (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_11 (A B C : S) : (sand C (sor C (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_12 (A B C : S) : (sand C (sor C (! B))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_13 (A B C : S) : (sor C (sand A (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_14 (A B C : S) : (sor C (snot A (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_15 (A B C : S) : (sor C (snot B (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_16 (A B C : S) : (sor C (sand C (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_17 (A B C : S) : (sor C (sand C (! B))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_18 (A B C : S) : (sxor (sand A (! A)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_19 (A B C : S) : (snot C (snot (! A) C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_20 (A B C : S) : (snot C (snot (! B) C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_21 (A B C : S) : (snot C (sand A (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_22 (A B C : S) : (snot C (sand A (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_23 (A B C : S) : (snot C (sand B (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_24 (A B C : S) : (sxor C (sand A (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_25 (A B C : S) : (sinside (sor A (! A)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_26 (A B C : S) : (sinside (sor C (! A)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_27 (A B C : S) : (sinside (sor C (! B)) C) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_28 (A B C : S) : (sinside C (sor A (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_29 (A B C : S) : (sinside C (sor C (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_30 (A B C : S) : (sinside C (sor C (! B))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_31 (A B C : S) : (soutside C (snot (! A) C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_32 (A B C : S) : (soutside C (snot (! B) C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_33 (A B C : S) : (soutside C (sand A (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_34 (A B C : S) : (soutside C (sand A (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_35 (A B C : S) : (soutside C (sand B (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_36 (A B C : S) : (snot (sor A (! A)) (! C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_37 (A B C : S) : (sxor (sor A (! A)) (! C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_38 (A B C : S) : (snot (sor C (! A)) (! C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_39 (A B C : S) : (snot (sor C (! B)) (! C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_40 (A B C : S) : (sxor (! C) (sor A (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_41 (A B C : S) : (sxor (sxor (! A) (! C)) A) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_42 (A B C : S) : (sxor (sxor (! B) (! C)) B) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_43 (A B C : S) : (sxor (sxor A (! C)) (! A)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_44 (A B C : S) : (sxor (sxor B (! C)) (! B)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_45 (A B C : S) : (sxor (! A) (sxor A (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_46 (A B C : S) : (sxor (! B) (sxor B (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_47 (A B C : S) : (sxor A (sxor (! A) (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_48 (A B C : S) : (sxor B (sxor (! B) (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_49 (A B C : S) : (soutside (sor A (! A)) (! C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_50 (A B C : S) : (soutside (sor C (! A)) (! C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_51 (A B C : S) : (soutside (sor C (! B)) (! C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_52 (A B C : S) : (sxor (snot (! A) C) (sor C (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_53 (A B C : S) : (sxor (snot (! B) C) (sor C (! B))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_54 (A B C : S) : (sor (snot A (! C)) (sand C (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_55 (A B C : S) : (sor (snot B (! C)) (sand C (! B))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_56 (A B C : S) : (sor (sand C (! A)) (snot A (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_57 (A B C : S) : (sor (sand C (! B)) (snot B (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_58 (A B C : S) : (snot (sor C (! A)) (snot (! A) C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_59 (A B C : S) : (sxor (sor C (! A)) (snot (! A) C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_60 (A B C : S) : (snot (sor C (! B)) (snot (! B) C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_61 (A B C : S) : (sxor (sor C (! B)) (snot (! B) C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_62 (A B C : S) : (sxor (snot A (! C)) (sand C (! A))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_63 (A B C : S) : (sxor (snot B (! C)) (sand C (! B))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_64 (A B C : S) : (sxor (sand C (! A)) (snot A (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_65 (A B C : S) : (sxor (sand C (! B)) (snot B (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_66 (A B C : S) : (soutside (sor C (! A)) (snot (! A) C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_67 (A B C : S) : (soutside (sor C (! B)) (snot (! B) C)) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_68 (A B C : S) : (sxor (sor (! A) (! C)) (sor A (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_69 (A B C : S) : (sxor (sor (! B) (! C)) (sor B (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_70 (A B C : S) : (sxor (sor A (! C)) (sor (! A) (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl
theorem e3_C_71 (A B C : S) : (sxor (sor B (! C)) (sor (! B) (! C))) = C := by cases A <;> cases B <;> cases C <;> rfl

theorem e4_0_0 (A B C D : S) : (sand (! A) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1 (A B C D : S) : (sand (! B) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2 (A B C D : S) : (sand (! C) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_3 (A B C D : S) : (sand (! D) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_4 (A B C D : S) : (sand A (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_5 (A B C D : S) : (sand B (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_6 (A B C D : S) : (sand C (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_7 (A B C D : S) : (sand D (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_8 (A B C D : S) : (sinside (! A) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_9 (A B C D : S) : (sinside (! B) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_10 (A B C D : S) : (sinside (! C) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_11 (A B C D : S) : (sinside (! D) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_12 (A B C D : S) : (sinside A (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_13 (A B C D : S) : (sinside B (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_14 (A B C D : S) : (sinside C (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_15 (A B C D : S) : (sinside D (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_16 (A B C D : S) : (snot (! A) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_17 (A B C D : S) : (sxor (! A) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_18 (A B C D : S) : (snot (! B) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_19 (A B C D : S) : (sxor (! B) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_20 (A B C D : S) : (snot (! C) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_21 (A B C D : S) : (sxor (! C) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_22 (A B C D : S) : (snot (! D) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_23 (A B C D : S) : (sxor (! D) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_24 (A B C D : S) : (soutside (! A) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_25 (A B C D : S) : (soutside (! B) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_26 (A B C D : S) : (soutside (! C) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_27 (A B C D : S) : (soutside (! D) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_28 (A B C D : S) : (snot A (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_29 (A B C D : S) : (snot A (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_30 (A B C D : S) : (snot A (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_31 (A B C D : S) : (snot A (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_32 (A B C D : S) : (snot B (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_33 (A B C D : S) : (snot B (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_34 (A B C D : S) : (snot B (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_35 (A B C D : S) : (snot B (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_36 (A B C D : S) : (snot C (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_37 (A B C D : S) : (snot C (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_38 (A B C D : S) : (snot C (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_39 (A B C D : S) : (snot C (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_40 (A B C D : S) : (snot D (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_41 (A B C D : S) : (snot D (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_42 (A B C D : S) : (snot D (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_43 (A B C D : S) : (snot D (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_44 (A B C D : S) : (sand (snot (! A) B) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_45 (A B C D : S) : (sand (snot (! A) B) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_46 (A B C D : S) : (sand (snot (! A) C) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_47 (A B C D : S) : (sand (snot (! A) C) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_48 (A B C D : S) : (sand (snot (! A) D) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_49 (A B C D : S) : (sand (snot (! A) D) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_50 (A B C D : S) : (sand (snot (! B) C) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_51 (A B C D : S) : (sand (snot (! B) C) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_52 (A B C D : S) : (sand (snot (! B) D) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_53 (A B C D : S) : (sand (snot (! B) D) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_54 (A B C D : S) : (sand (snot (! C) D) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_55 (A B C D : S) : (sand (snot (! C) D) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_56 (A B C D : S) : (sand (sand A (! A)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_57 (A B C D : S) : (sand (sand A (! A)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_58 (A B C D : S) : (sand (sand A (! A)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_59 (A B C D : S) : (sand (sand A (! A)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_60 (A B C D : S) : (snot (sand A (! A)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_61 (A B C D : S) : (snot (sand A (! A)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_62 (A B C D : S) : (snot (sand A (! A)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_63 (A B C D : S) : (snot (sand A (! A)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_64 (A B C D : S) : (sand (sand A (! B)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_65 (A B C D : S) : (snot (sand A (! B)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_66 (A B C D : S) : (sand (sand A (! C)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_67 (A B C D : S) : (snot (sand A (! C)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_68 (A B C D : S) : (sand (sand A (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_69 (A B C D : S) : (snot (sand A (! D)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_70 (A B C D : S) : (snot (snot A (! B)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_71 (A B C D : S) : (snot (snot A (! B)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_72 (A B C D : S) : (snot (snot A (! C)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_73 (A B C D : S) : (snot (snot A (! C)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_74 (A B C D : S) : (snot (snot A (! D)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_75 (A B C D : S) : (snot (snot A (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_76 (A B C D : S) : (sand (sand B (! A)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_77 (A B C D : S) : (snot (sand B (! A)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_78 (A B C D : S) : (sand (sand B (! C)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_79 (A B C D : S) : (snot (sand B (! C)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_80 (A B C D : S) : (sand (sand B (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_81 (A B C D : S) : (snot (sand B (! D)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_82 (A B C D : S) : (snot (snot B (! C)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_83 (A B C D : S) : (snot (snot B (! C)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_84 (A B C D : S) : (snot (snot B (! D)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_85 (A B C D : S) : (snot (snot B (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_86 (A B C D : S) : (sand (sand C (! A)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_87 (A B C D : S) : (snot (sand C (! A)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_88 (A B C D : S) : (sand (sand C (! B)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_89 (A B C D : S) : (snot (sand C (! B)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_90 (A B C D : S) : (sand (sand C (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_91 (A B C D : S) : (snot (sand C (! D)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_92 (A B C D : S) : (snot (snot C (! D)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_93 (A B C D : S) : (snot (snot C (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_94 (A B C D : S) : (sand (sand D (! A)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_95 (A B C D : S) : (snot (sand D (! A)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_96 (A B C D : S) : (sand (sand D (! B)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_97 (A B C D : S) : (snot (sand D (! B)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_98 (A B C D : S) : (sand (sand D (! C)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_99 (A B C D : S) : (snot (sand D (! C)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_100 (A B C D : S) : (sand A (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_101 (A B C D : S) : (sand A (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_102 (A B C D : S) : (sand A (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_103 (A B C D : S) : (sand A (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_104 (A B C D : S) : (sand A (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_105 (A B C D : S) : (sand A (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_106 (A B C D : S) : (sand A (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_107 (A B C D : S) : (sand B (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_108 (A B C D : S) : (sand B (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_109 (A B C D : S) : (sand B (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_110 (A B C D : S) : (sand B (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_111 (A B C D : S) : (sand B (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_112 (A B C D : S) : (sand B (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_113 (A B C D : S) : (sand B (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_114 (A B C D : S) : (sand C (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_115 (A B C D : S) : (sand C (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_116 (A B C D : S) : (sand C (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_117 (A B C D : S) : (sand C (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_118 (A B C D : S) : (sand C (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_119 (A B C D : S) : (sand C (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_120 (A B C D : S) : (sand C (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_121 (A B C D : S) : (sand D (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_122 (A B C D : S) : (sand D (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_123 (A B C D : S) : (sand D (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_124 (A B C D : S) : (sand D (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_125 (A B C D : S) : (sand D (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_126 (A B C D : S) : (sand D (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_127 (A B C D : S) : (sand D (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_128 (A B C D : S) : (sinside (snot (! A) B) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_129 (A B C D : S) : (sinside (snot (! A) B) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_130 (A B C D : S) : (sinside (snot (! A) C) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_131 (A B C D : S) : (sinside (snot (! A) C) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_132 (A B C D : S) : (sinside (snot (! A) D) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_133 (A B C D : S) : (sinside (snot (! A) D) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_134 (A B C D : S) : (sinside (snot (! B) C) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_135 (A B C D : S) : (sinside (snot (! B) C) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_136 (A B C D : S) : (sinside (snot (! B) D) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_137 (A B C D : S) : (sinside (snot (! B) D) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_138 (A B C D : S) : (sinside (snot (! C) D) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_139 (A B C D : S) : (sinside (snot (! C) D) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_140 (A B C D : S) : (sinside (sand A (! A)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_141 (A B C D : S) : (sinside (sand A (! A)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_142 (A B C D : S) : (sinside (sand A (! A)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_143 (A B C D : S) : (sinside (sand A (! A)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_144 (A B C D : S) : (sinside (sand A (! B)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_145 (A B C D : S) : (sinside (sand A (! C)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_146 (A B C D : S) : (sinside (sand A (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_147 (A B C D : S) : (sinside (sand B (! A)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_148 (A B C D : S) : (sinside (sand B (! C)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_149 (A B C D : S) : (sinside (sand B (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_150 (A B C D : S) : (sinside (sand C (! A)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_151 (A B C D : S) : (sinside (sand C (! B)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_152 (A B C D : S) : (sinside (sand C (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_153 (A B C D : S) : (sinside (sand D (! A)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_154 (A B C D : S) : (sinside (sand D (! B)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_155 (A B C D : S) : (sinside (sand D (! C)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_156 (A B C D : S) : (sinside A (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_157 (A B C D : S) : (sinside A (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_158 (A B C D : S) : (sinside A (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_159 (A B C D : S) : (sinside A (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_160 (A B C D : S) : (sinside A (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_161 (A B C D : S) : (sinside A (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_162 (A B C D : S) : (sinside A (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_163 (A B C D : S) : (soutside A (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_164 (A B C D : S) : (soutside A (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_165 (A B C D : S) : (soutside A (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_166 (A B C D : S) : (soutside A (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_167 (A B C D : S) : (sinside B (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_168 (A B C D : S) : (sinside B (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_169 (A B C D : S) : (sinside B (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_170 (A B C D : S) : (sinside B (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_171 (A B C D : S) : (sinside B (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_172 (A B C D : S) : (sinside B (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_173 (A B C D : S) : (sinside B (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_174 (A B C D : S) : (soutside B (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_175 (A B C D : S) : (soutside B (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_176 (A B C D : S) : (soutside B (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_177 (A B C D : S) : (soutside B (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_178 (A B C D : S) : (sinside C (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_179 (A B C D : S) : (sinside C (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_180 (A B C D : S) : (sinside C (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_181 (A B C D : S) : (sinside C (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_182 (A B C D : S) : (sinside C (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_183 (A B C D : S) : (sinside C (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_184 (A B C D : S) : (sinside C (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_185 (A B C D : S) : (soutside C (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_186 (A B C D : S) : (soutside C (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_187 (A B C D : S) : (soutside C (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_188 (A B C D : S) : (soutside C (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_189 (A B C D : S) : (sinside D (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_190 (A B C D : S) : (sinside D (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_191 (A B C D : S) : (sinside D (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_192 (A B C D : S) : (sinside D (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_193 (A B C D : S) : (sinside D (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_194 (A B C D : S) : (sinside D (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_195 (A B C D : S) : (sinside D (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_196 (A B C D : S) : (soutside D (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_197 (A B C D : S) : (soutside D (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_198 (A B C D : S) : (soutside D (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_199 (A B C D : S) : (soutside D (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_200 (A B C D : S) : (soutside (sand A (! A)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_201 (A B C D : S) : (soutside (sand A (! A)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_202 (A B C D : S) : (soutside (sand A (! A)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_203 (A B C D : S) : (soutside (sand A (! A)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_204 (A B C D : S) : (soutside (sand A (! B)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_205 (A B C D : S) : (soutside (sand A (! C)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_206 (A B C D : S) : (soutside (sand A (! D)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_207 (A B C D : S) : (soutside (snot A (! B)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_208 (A B C D : S) : (soutside (snot A (! B)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_209 (A B C D : S) : (soutside (snot A (! C)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_210 (A B C D : S) : (soutside (snot A (! C)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_211 (A B C D : S) : (soutside (snot A (! D)) A) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_212 (A B C D : S) : (soutside (snot A (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_213 (A B C D : S) : (soutside (sand B (! A)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_214 (A B C D : S) : (soutside (sand B (! C)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_215 (A B C D : S) : (soutside (sand B (! D)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_216 (A B C D : S) : (soutside (snot B (! C)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_217 (A B C D : S) : (soutside (snot B (! C)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_218 (A B C D : S) : (soutside (snot B (! D)) B) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_219 (A B C D : S) : (soutside (snot B (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_220 (A B C D : S) : (soutside (sand C (! A)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_221 (A B C D : S) : (soutside (sand C (! B)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_222 (A B C D : S) : (soutside (sand C (! D)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_223 (A B C D : S) : (soutside (snot C (! D)) C) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_224 (A B C D : S) : (soutside (snot C (! D)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_225 (A B C D : S) : (soutside (sand D (! A)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_226 (A B C D : S) : (soutside (sand D (! B)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_227 (A B C D : S) : (soutside (sand D (! C)) D) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_228 (A B C D : S) : (snot (! A) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_229 (A B C D : S) : (snot (! A) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_230 (A B C D : S) : (snot (! A) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_231 (A B C D : S) : (snot (! A) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_232 (A B C D : S) : (snot (! B) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_233 (A B C D : S) : (snot (! B) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_234 (A B C D : S) : (snot (! B) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_235 (A B C D : S) : (snot (! B) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_236 (A B C D : S) : (snot (! C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_237 (A B C D : S) : (snot (! C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_238 (A B C D : S) : (snot (! C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_239 (A B C D : S) : (snot (! C) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_240 (A B C D : S) : (snot (! D) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_241 (A B C D : S) : (snot (! D) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_242 (A B C D : S) : (snot (! D) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_243 (A B C D : S) : (snot (! D) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_244 (A B C D : S) : (snot (snot (! A) B) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_245 (A B C D : S) : (snot (snot (! A) B) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_246 (A B C D : S) : (snot (snot (! A) C) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_247 (A B C D : S) : (snot (snot (! A) C) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_248 (A B C D : S) : (snot (snot (! A) D) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_249 (A B C D : S) : (snot (snot (! A) D) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_250 (A B C D : S) : (snot (snot (! B) C) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_251 (A B C D : S) : (snot (snot (! B) C) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_252 (A B C D : S) : (snot (snot (! B) D) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_253 (A B C D : S) : (snot (snot (! B) D) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_254 (A B C D : S) : (snot (snot (! C) D) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_255 (A B C D : S) : (snot (snot (! C) D) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_256 (A B C D : S) : (sand (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_257 (A B C D : S) : (sand (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_258 (A B C D : S) : (sand (sand A (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_259 (A B C D : S) : (sand (sand A (! A)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_260 (A B C D : S) : (snot (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_261 (A B C D : S) : (snot (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_262 (A B C D : S) : (snot (sand A (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_263 (A B C D : S) : (snot (sand A (! A)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_264 (A B C D : S) : (sand (sand A (! B)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_265 (A B C D : S) : (snot (sand A (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_266 (A B C D : S) : (sand (sand A (! C)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_267 (A B C D : S) : (snot (sand A (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_268 (A B C D : S) : (sand (sand A (! D)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_269 (A B C D : S) : (snot (sand A (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_270 (A B C D : S) : (sand (snot A (! B)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_271 (A B C D : S) : (sand (snot A (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_272 (A B C D : S) : (sand (snot A (! C)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_273 (A B C D : S) : (sand (snot A (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_274 (A B C D : S) : (sand (snot A (! D)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_275 (A B C D : S) : (sand (snot A (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_276 (A B C D : S) : (sand (sand B (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_277 (A B C D : S) : (snot (sand B (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_278 (A B C D : S) : (sand (sand B (! C)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_279 (A B C D : S) : (snot (sand B (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_280 (A B C D : S) : (sand (sand B (! D)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_281 (A B C D : S) : (snot (sand B (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_282 (A B C D : S) : (sand (snot B (! C)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_283 (A B C D : S) : (sand (snot B (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_284 (A B C D : S) : (sand (snot B (! D)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_285 (A B C D : S) : (sand (snot B (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_286 (A B C D : S) : (sand (sand C (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_287 (A B C D : S) : (snot (sand C (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_288 (A B C D : S) : (sand (sand C (! B)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_289 (A B C D : S) : (snot (sand C (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_290 (A B C D : S) : (sand (sand C (! D)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_291 (A B C D : S) : (snot (sand C (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_292 (A B C D : S) : (sand (snot C (! D)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_293 (A B C D : S) : (sand (snot C (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_294 (A B C D : S) : (sand (sand D (! A)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_295 (A B C D : S) : (snot (sand D (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_296 (A B C D : S) : (sand (sand D (! B)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_297 (A B C D : S) : (snot (sand D (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_298 (A B C D : S) : (sand (sand D (! C)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_299 (A B C D : S) : (snot (sand D (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_300 (A B C D : S) : (sand (! A) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_301 (A B C D : S) : (sand (! A) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_302 (A B C D : S) : (sand (! A) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_303 (A B C D : S) : (sand (! A) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_304 (A B C D : S) : (sand (! A) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_305 (A B C D : S) : (sand (! A) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_306 (A B C D : S) : (sand (! A) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_307 (A B C D : S) : (sand (! B) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_308 (A B C D : S) : (sand (! B) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_309 (A B C D : S) : (sand (! B) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_310 (A B C D : S) : (sand (! B) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_311 (A B C D : S) : (sand (! B) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_312 (A B C D : S) : (sand (! B) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_313 (A B C D : S) : (sand (! B) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_314 (A B C D : S) : (sand (! C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_315 (A B C D : S) : (sand (! C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_316 (A B C D : S) : (sand (! C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_317 (A B C D : S) : (sand (! C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_318 (A B C D : S) : (sand (! C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_319 (A B C D : S) : (sand (! C) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_320 (A B C D : S) : (sand (! C) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_321 (A B C D : S) : (sand (! D) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_322 (A B C D : S) : (sand (! D) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_323 (A B C D : S) : (sand (! D) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_324 (A B C D : S) : (sand (! D) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_325 (A B C D : S) : (sand (! D) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_326 (A B C D : S) : (sand (! D) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_327 (A B C D : S) : (sand (! D) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_328 (A B C D : S) : (sinside (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_329 (A B C D : S) : (sinside (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_330 (A B C D : S) : (sinside (sand A (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_331 (A B C D : S) : (sinside (sand A (! A)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_332 (A B C D : S) : (sinside (sand A (! B)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_333 (A B C D : S) : (sinside (sand A (! C)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_334 (A B C D : S) : (sinside (sand A (! D)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_335 (A B C D : S) : (sinside (snot A (! B)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_336 (A B C D : S) : (sinside (snot A (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_337 (A B C D : S) : (sinside (snot A (! C)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_338 (A B C D : S) : (sinside (snot A (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_339 (A B C D : S) : (sinside (snot A (! D)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_340 (A B C D : S) : (sinside (snot A (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_341 (A B C D : S) : (sinside (sand B (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_342 (A B C D : S) : (sinside (sand B (! C)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_343 (A B C D : S) : (sinside (sand B (! D)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_344 (A B C D : S) : (sinside (snot B (! C)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_345 (A B C D : S) : (sinside (snot B (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_346 (A B C D : S) : (sinside (snot B (! D)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_347 (A B C D : S) : (sinside (snot B (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_348 (A B C D : S) : (sinside (sand C (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_349 (A B C D : S) : (sinside (sand C (! B)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_350 (A B C D : S) : (sinside (sand C (! D)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_351 (A B C D : S) : (sinside (snot C (! D)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_352 (A B C D : S) : (sinside (snot C (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_353 (A B C D : S) : (sinside (sand D (! A)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_354 (A B C D : S) : (sinside (sand D (! B)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_355 (A B C D : S) : (sinside (sand D (! C)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_356 (A B C D : S) : (sinside (! A) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_357 (A B C D : S) : (sinside (! A) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_358 (A B C D : S) : (sinside (! A) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_359 (A B C D : S) : (sinside (! A) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_360 (A B C D : S) : (sinside (! A) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_361 (A B C D : S) : (sinside (! A) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_362 (A B C D : S) : (sinside (! A) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_363 (A B C D : S) : (soutside (! A) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_364 (A B C D : S) : (soutside (! A) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_365 (A B C D : S) : (soutside (! A) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_366 (A B C D : S) : (soutside (! A) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_367 (A B C D : S) : (sinside (! B) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_368 (A B C D : S) : (sinside (! B) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_369 (A B C D : S) : (sinside (! B) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_370 (A B C D : S) : (sinside (! B) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_371 (A B C D : S) : (sinside (! B) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_372 (A B C D : S) : (sinside (! B) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_373 (A B C D : S) : (sinside (! B) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_374 (A B C D : S) : (soutside (! B) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_375 (A B C D : S) : (soutside (! B) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_376 (A B C D : S) : (soutside (! B) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_377 (A B C D : S) : (soutside (! B) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_378 (A B C D : S) : (sinside (! C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_379 (A B C D : S) : (sinside (! C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_380 (A B C D : S) : (sinside (! C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_381 (A B C D : S) : (sinside (! C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_382 (A B C D : S) : (sinside (! C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_383 (A B C D : S) : (sinside (! C) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_384 (A B C D : S) : (sinside (! C) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_385 (A B C D : S) : (soutside (! C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_386 (A B C D : S) : (soutside (! C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_387 (A B C D : S) : (soutside (! C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_388 (A B C D : S) : (soutside (! C) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_389 (A B C D : S) : (sinside (! D) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_390 (A B C D : S) : (sinside (! D) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_391 (A B C D : S) : (sinside (! D) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_392 (A B C D : S) : (sinside (! D) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_393 (A B C D : S) : (sinside (! D) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_394 (A B C D : S) : (sinside (! D) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_395 (A B C D : S) : (sinside (! D) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_396 (A B C D : S) : (soutside (! D) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_397 (A B C D : S) : (soutside (! D) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_398 (A B C D : S) : (soutside (! D) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_399 (A B C D : S) : (soutside (! D) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_400 (A B C D : S) : (soutside (snot (! A) B) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_401 (A B C D : S) : (soutside (snot (! A) B) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_402 (A B C D : S) : (soutside (snot (! A) C) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_403 (A B C D : S) : (soutside (snot (! A) C) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_404 (A B C D : S) : (soutside (snot (! A) D) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_405 (A B C D : S) : (soutside (snot (! A) D) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_406 (A B C D : S) : (soutside (snot (! B) C) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_407 (A B C D : S) : (soutside (snot (! B) C) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_408 (A B C D : S) : (soutside (snot (! B) D) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_409 (A B C D : S) : (soutside (snot (! B) D) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_410 (A B C D : S) : (soutside (snot (! C) D) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_411 (A B C D : S) : (soutside (snot (! C) D) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_412 (A B C D : S) : (soutside (sand A (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_413 (A B C D : S) : (soutside (sand A (! A)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_414 (A B C D : S) : (soutside (sand A (! A)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_415 (A B C D : S) : (soutside (sand A (! A)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_416 (A B C D : S) : (soutside (sand A (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_417 (A B C D : S) : (soutside (sand A (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_418 (A B C D : S) : (soutside (sand A (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_419 (A B C D : S) : (soutside (sand B (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_420 (A B C D : S) : (soutside (sand B (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_421 (A B C D : S) : (soutside (sand B (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_422 (A B C D : S) : (soutside (sand C (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_423 (A B C D : S) : (soutside (sand C (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_424 (A B C D : S) : (soutside (sand C (! D)) (! D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_425 (A B C D : S) : (soutside (sand D (! A)) (! A)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_426 (A B C D : S) : (soutside (sand D (! B)) (! B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_427 (A B C D : S) : (soutside (sand D (! C)) (! C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_428 (A B C D : S) : (snot (! A) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_429 (A B C D : S) : (snot (! A) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_430 (A B C D : S) : (snot (! A) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_431 (A B C D : S) : (snot (! B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_432 (A B C D : S) : (snot (! B) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_433 (A B C D : S) : (snot (! B) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_434 (A B C D : S) : (snot (! C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_435 (A B C D : S) : (snot (! C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_436 (A B C D : S) : (snot (! C) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_437 (A B C D : S) : (snot (! D) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_438 (A B C D : S) : (snot (! D) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_439 (A B C D : S) : (snot (! D) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_440 (A B C D : S) : (snot (sor A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_441 (A B C D : S) : (sxor (sor A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_442 (A B C D : S) : (snot (sor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_443 (A B C D : S) : (snot (sor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_444 (A B C D : S) : (sxor (sor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_445 (A B C D : S) : (snot (sor A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_446 (A B C D : S) : (snot (sor A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_447 (A B C D : S) : (sxor (sor A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_448 (A B C D : S) : (snot (sor A (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_449 (A B C D : S) : (snot (sor A (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_450 (A B C D : S) : (sxor (sor A (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_451 (A B C D : S) : (snot (sor B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_452 (A B C D : S) : (snot (sor B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_453 (A B C D : S) : (sxor (sor B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_454 (A B C D : S) : (snot (sor B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_455 (A B C D : S) : (snot (sor B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_456 (A B C D : S) : (sxor (sor B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_457 (A B C D : S) : (snot (sor B (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_458 (A B C D : S) : (snot (sor B (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_459 (A B C D : S) : (sxor (sor B (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_460 (A B C D : S) : (snot (sor C (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_461 (A B C D : S) : (snot (sor C (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_462 (A B C D : S) : (sxor (sor C (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_463 (A B C D : S) : (snot (sor C (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_464 (A B C D : S) : (snot (sor C (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_465 (A B C D : S) : (sxor (sor C (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_466 (A B C D : S) : (snot (sor C (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_467 (A B C D : S) : (snot (sor C (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_468 (A B C D : S) : (sxor (sor C (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_469 (A B C D : S) : (snot (sor D (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_470 (A B C D : S) : (snot (sor D (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_471 (A B C D : S) : (sxor (sor D (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_472 (A B C D : S) : (snot (sor D (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_473 (A B C D : S) : (snot (sor D (! B)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_474 (A B C D : S) : (sxor (sor D (! B)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_475 (A B C D : S) : (snot (sor D (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_476 (A B C D : S) : (snot (sor D (! C)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_477 (A B C D : S) : (sxor (sor D (! C)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_478 (A B C D : S) : (snot (snot (! A) B) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_479 (A B C D : S) : (snot (snot (! A) B) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_480 (A B C D : S) : (snot (snot (! A) B) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_481 (A B C D : S) : (snot (snot (! A) B) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_482 (A B C D : S) : (snot (snot (! A) B) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_483 (A B C D : S) : (snot (snot (! A) B) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_484 (A B C D : S) : (snot (snot (! A) B) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_485 (A B C D : S) : (snot (snot (! A) C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_486 (A B C D : S) : (snot (snot (! A) C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_487 (A B C D : S) : (snot (snot (! A) C) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_488 (A B C D : S) : (snot (snot (! A) C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_489 (A B C D : S) : (snot (snot (! A) C) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_490 (A B C D : S) : (snot (snot (! A) C) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_491 (A B C D : S) : (snot (snot (! A) C) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_492 (A B C D : S) : (snot (snot (! A) D) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_493 (A B C D : S) : (snot (snot (! A) D) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_494 (A B C D : S) : (snot (snot (! A) D) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_495 (A B C D : S) : (snot (snot (! A) D) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_496 (A B C D : S) : (snot (snot (! A) D) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_497 (A B C D : S) : (snot (snot (! A) D) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_498 (A B C D : S) : (snot (snot (! A) D) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_499 (A B C D : S) : (snot (snot (! B) C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_500 (A B C D : S) : (snot (snot (! B) C) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_501 (A B C D : S) : (snot (snot (! B) C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_502 (A B C D : S) : (snot (snot (! B) C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_503 (A B C D : S) : (snot (snot (! B) C) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_504 (A B C D : S) : (snot (snot (! B) C) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_505 (A B C D : S) : (snot (snot (! B) C) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_506 (A B C D : S) : (snot (snot (! B) D) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_507 (A B C D : S) : (snot (snot (! B) D) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_508 (A B C D : S) : (snot (snot (! B) D) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_509 (A B C D : S) : (snot (snot (! B) D) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_510 (A B C D : S) : (snot (snot (! B) D) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_511 (A B C D : S) : (snot (snot (! B) D) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_512 (A B C D : S) : (snot (snot (! B) D) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_513 (A B C D : S) : (snot (snot (! C) D) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_514 (A B C D : S) : (snot (snot (! C) D) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_515 (A B C D : S) : (snot (snot (! C) D) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_516 (A B C D : S) : (snot (snot (! C) D) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_517 (A B C D : S) : (snot (snot (! C) D) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_518 (A B C D : S) : (snot (snot (! C) D) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_519 (A B C D : S) : (snot (snot (! C) D) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_520 (A B C D : S) : (sand (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_521 (A B C D : S) : (sand (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_522 (A B C D : S) : (sand (sand A (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_523 (A B C D : S) : (sand (sand A (! A)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_524 (A B C D : S) : (sand (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_525 (A B C D : S) : (sand (sand A (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_526 (A B C D : S) : (sand (sand A (! A)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_527 (A B C D : S) : (sand (sand A (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_528 (A B C D : S) : (sand (sand A (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_529 (A B C D : S) : (sand (sand A (! A)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_530 (A B C D : S) : (sand (sand A (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_531 (A B C D : S) : (sand (sand A (! A)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_532 (A B C D : S) : (sand (sand A (! A)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_533 (A B C D : S) : (snot (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_534 (A B C D : S) : (snot (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_535 (A B C D : S) : (snot (sand A (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_536 (A B C D : S) : (snot (sand A (! A)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_537 (A B C D : S) : (snot (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_538 (A B C D : S) : (snot (sand A (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_539 (A B C D : S) : (snot (sand A (! A)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_540 (A B C D : S) : (snot (sand A (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_541 (A B C D : S) : (snot (sand A (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_542 (A B C D : S) : (snot (sand A (! A)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_543 (A B C D : S) : (snot (sand A (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_544 (A B C D : S) : (snot (sand A (! A)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_545 (A B C D : S) : (snot (sand A (! A)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_546 (A B C D : S) : (sor (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_547 (A B C D : S) : (sand (sand A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_548 (A B C D : S) : (snot (sand A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_549 (A B C D : S) : (snot (sand A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_550 (A B C D : S) : (snot (sand A (! B)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_551 (A B C D : S) : (snot (sand A (! B)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_552 (A B C D : S) : (snot (sand A (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_553 (A B C D : S) : (snot (sand A (! B)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_554 (A B C D : S) : (sand (sand A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_555 (A B C D : S) : (snot (sand A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_556 (A B C D : S) : (snot (sand A (! C)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_557 (A B C D : S) : (snot (sand A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_558 (A B C D : S) : (snot (sand A (! C)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_559 (A B C D : S) : (snot (sand A (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_560 (A B C D : S) : (snot (sand A (! C)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_561 (A B C D : S) : (sand (sand A (! D)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_562 (A B C D : S) : (snot (sand A (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_563 (A B C D : S) : (snot (sand A (! D)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_564 (A B C D : S) : (snot (sand A (! D)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_565 (A B C D : S) : (snot (sand A (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_566 (A B C D : S) : (snot (sand A (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_567 (A B C D : S) : (snot (sand A (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_568 (A B C D : S) : (snot (snot A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_569 (A B C D : S) : (snot (snot A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_570 (A B C D : S) : (snot (snot A (! B)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_571 (A B C D : S) : (snot (snot A (! B)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_572 (A B C D : S) : (snot (snot A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_573 (A B C D : S) : (snot (snot A (! B)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_574 (A B C D : S) : (snot (snot A (! B)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_575 (A B C D : S) : (snot (snot A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_576 (A B C D : S) : (snot (snot A (! C)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_577 (A B C D : S) : (snot (snot A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_578 (A B C D : S) : (snot (snot A (! C)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_579 (A B C D : S) : (snot (snot A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_580 (A B C D : S) : (snot (snot A (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_581 (A B C D : S) : (snot (snot A (! C)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_582 (A B C D : S) : (snot (snot A (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_583 (A B C D : S) : (snot (snot A (! D)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_584 (A B C D : S) : (snot (snot A (! D)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_585 (A B C D : S) : (snot (snot A (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_586 (A B C D : S) : (snot (snot A (! D)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_587 (A B C D : S) : (snot (snot A (! D)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_588 (A B C D : S) : (snot (snot A (! D)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_589 (A B C D : S) : (sand (sor A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_590 (A B C D : S) : (sand (sor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_591 (A B C D : S) : (sand (sor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_592 (A B C D : S) : (sand (sor A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_593 (A B C D : S) : (sand (sor A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_594 (A B C D : S) : (sand (sor A (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_595 (A B C D : S) : (sand (sor A (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_596 (A B C D : S) : (snot (sxor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_597 (A B C D : S) : (snot (sxor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_598 (A B C D : S) : (snot (sxor A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_599 (A B C D : S) : (snot (sxor A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_600 (A B C D : S) : (snot (sxor A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_601 (A B C D : S) : (snot (sxor A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_602 (A B C D : S) : (snot (sxor A (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_603 (A B C D : S) : (snot (sxor A (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_604 (A B C D : S) : (snot (sxor A (! D)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_605 (A B C D : S) : (sand (sand B (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_606 (A B C D : S) : (snot (sand B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_607 (A B C D : S) : (snot (sand B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_608 (A B C D : S) : (snot (sand B (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_609 (A B C D : S) : (snot (sand B (! A)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_610 (A B C D : S) : (snot (sand B (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_611 (A B C D : S) : (snot (sand B (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_612 (A B C D : S) : (sand (sand B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_613 (A B C D : S) : (snot (sand B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_614 (A B C D : S) : (snot (sand B (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_615 (A B C D : S) : (snot (sand B (! C)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_616 (A B C D : S) : (snot (sand B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_617 (A B C D : S) : (snot (sand B (! C)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_618 (A B C D : S) : (snot (sand B (! C)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_619 (A B C D : S) : (sand (sand B (! D)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_620 (A B C D : S) : (snot (sand B (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_621 (A B C D : S) : (snot (sand B (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_622 (A B C D : S) : (snot (sand B (! D)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_623 (A B C D : S) : (snot (sand B (! D)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_624 (A B C D : S) : (snot (sand B (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_625 (A B C D : S) : (snot (sand B (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_626 (A B C D : S) : (snot (snot B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_627 (A B C D : S) : (snot (snot B (! C)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_628 (A B C D : S) : (snot (snot B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_629 (A B C D : S) : (snot (snot B (! C)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_630 (A B C D : S) : (snot (snot B (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_631 (A B C D : S) : (snot (snot B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_632 (A B C D : S) : (snot (snot B (! C)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_633 (A B C D : S) : (snot (snot B (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_634 (A B C D : S) : (snot (snot B (! D)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_635 (A B C D : S) : (snot (snot B (! D)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_636 (A B C D : S) : (snot (snot B (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_637 (A B C D : S) : (snot (snot B (! D)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_638 (A B C D : S) : (snot (snot B (! D)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_639 (A B C D : S) : (snot (snot B (! D)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_640 (A B C D : S) : (sand (sor B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_641 (A B C D : S) : (sand (sor B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_642 (A B C D : S) : (sand (sor B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_643 (A B C D : S) : (sand (sor B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_644 (A B C D : S) : (sand (sor B (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_645 (A B C D : S) : (sand (sor B (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_646 (A B C D : S) : (snot (sxor B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_647 (A B C D : S) : (snot (sxor B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_648 (A B C D : S) : (snot (sxor B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_649 (A B C D : S) : (snot (sxor B (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_650 (A B C D : S) : (snot (sxor B (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_651 (A B C D : S) : (snot (sxor B (! D)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_652 (A B C D : S) : (sand (sand C (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_653 (A B C D : S) : (snot (sand C (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_654 (A B C D : S) : (snot (sand C (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_655 (A B C D : S) : (snot (sand C (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_656 (A B C D : S) : (snot (sand C (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_657 (A B C D : S) : (snot (sand C (! A)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_658 (A B C D : S) : (snot (sand C (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_659 (A B C D : S) : (sand (sand C (! B)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_660 (A B C D : S) : (snot (sand C (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_661 (A B C D : S) : (snot (sand C (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_662 (A B C D : S) : (snot (sand C (! B)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_663 (A B C D : S) : (snot (sand C (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_664 (A B C D : S) : (snot (sand C (! B)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_665 (A B C D : S) : (snot (sand C (! B)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_666 (A B C D : S) : (sand (sand C (! D)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_667 (A B C D : S) : (snot (sand C (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_668 (A B C D : S) : (snot (sand C (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_669 (A B C D : S) : (snot (sand C (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_670 (A B C D : S) : (snot (sand C (! D)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_671 (A B C D : S) : (snot (sand C (! D)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_672 (A B C D : S) : (snot (sand C (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_673 (A B C D : S) : (snot (snot C (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_674 (A B C D : S) : (snot (snot C (! D)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_675 (A B C D : S) : (snot (snot C (! D)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_676 (A B C D : S) : (snot (snot C (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_677 (A B C D : S) : (snot (snot C (! D)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_678 (A B C D : S) : (snot (snot C (! D)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_679 (A B C D : S) : (snot (snot C (! D)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_680 (A B C D : S) : (sand (sor C (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_681 (A B C D : S) : (sand (sor C (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_682 (A B C D : S) : (sand (sor C (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_683 (A B C D : S) : (sand (sor C (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_684 (A B C D : S) : (sand (sor C (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_685 (A B C D : S) : (sand (sor C (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_686 (A B C D : S) : (snot (sxor C (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_687 (A B C D : S) : (snot (sxor C (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_688 (A B C D : S) : (snot (sxor C (! D)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_689 (A B C D : S) : (sand (sand D (! A)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_690 (A B C D : S) : (snot (sand D (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_691 (A B C D : S) : (snot (sand D (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_692 (A B C D : S) : (snot (sand D (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_693 (A B C D : S) : (snot (sand D (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_694 (A B C D : S) : (snot (sand D (! A)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_695 (A B C D : S) : (snot (sand D (! A)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_696 (A B C D : S) : (sand (sand D (! B)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_697 (A B C D : S) : (snot (sand D (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_698 (A B C D : S) : (snot (sand D (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_699 (A B C D : S) : (snot (sand D (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_700 (A B C D : S) : (snot (sand D (! B)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_701 (A B C D : S) : (snot (sand D (! B)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_702 (A B C D : S) : (snot (sand D (! B)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_703 (A B C D : S) : (sand (sand D (! C)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_704 (A B C D : S) : (snot (sand D (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_705 (A B C D : S) : (snot (sand D (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_706 (A B C D : S) : (snot (sand D (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_707 (A B C D : S) : (snot (sand D (! C)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_708 (A B C D : S) : (snot (sand D (! C)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_709 (A B C D : S) : (snot (sand D (! C)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_710 (A B C D : S) : (sand (sor D (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_711 (A B C D : S) : (sand (sor D (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_712 (A B C D : S) : (sand (sor D (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_713 (A B C D : S) : (sand (sor D (! B)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_714 (A B C D : S) : (sand (sor D (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_715 (A B C D : S) : (sand (sor D (! C)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_716 (A B C D : S) : (sand (snot (! A) B) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_717 (A B C D : S) : (sand (snot (! A) B) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_718 (A B C D : S) : (sand (snot (! A) B) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_719 (A B C D : S) : (sand (snot (! A) B) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_720 (A B C D : S) : (sand (snot (! A) B) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_721 (A B C D : S) : (sand (snot (! A) B) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_722 (A B C D : S) : (sand (snot (! A) B) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_723 (A B C D : S) : (sand (snot (! A) B) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_724 (A B C D : S) : (sand (snot (! A) B) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_725 (A B C D : S) : (sand (snot (! A) B) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_726 (A B C D : S) : (sand (snot (! A) B) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_727 (A B C D : S) : (sand (snot (! A) B) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_728 (A B C D : S) : (snot (snot (! A) B) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_729 (A B C D : S) : (snot (snot (! A) B) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_730 (A B C D : S) : (sxor (snot (! A) B) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_731 (A B C D : S) : (sand (snot (! A) C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_732 (A B C D : S) : (sand (snot (! A) C) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_733 (A B C D : S) : (sand (snot (! A) C) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_734 (A B C D : S) : (sand (snot (! A) C) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_735 (A B C D : S) : (sand (snot (! A) C) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_736 (A B C D : S) : (sand (snot (! A) C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_737 (A B C D : S) : (sand (snot (! A) C) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_738 (A B C D : S) : (sand (snot (! A) C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_739 (A B C D : S) : (sand (snot (! A) C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_740 (A B C D : S) : (sand (snot (! A) C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_741 (A B C D : S) : (sand (snot (! A) C) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_742 (A B C D : S) : (sand (snot (! A) C) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_743 (A B C D : S) : (snot (snot (! A) C) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_744 (A B C D : S) : (snot (snot (! A) C) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_745 (A B C D : S) : (sxor (snot (! A) C) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_746 (A B C D : S) : (sand (snot (! A) D) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_747 (A B C D : S) : (sand (snot (! A) D) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_748 (A B C D : S) : (sand (snot (! A) D) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_749 (A B C D : S) : (sand (snot (! A) D) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_750 (A B C D : S) : (sand (snot (! A) D) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_751 (A B C D : S) : (sand (snot (! A) D) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_752 (A B C D : S) : (sand (snot (! A) D) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_753 (A B C D : S) : (sand (snot (! A) D) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_754 (A B C D : S) : (sand (snot (! A) D) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_755 (A B C D : S) : (sand (snot (! A) D) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_756 (A B C D : S) : (sand (snot (! A) D) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_757 (A B C D : S) : (sand (snot (! A) D) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_758 (A B C D : S) : (snot (snot (! A) D) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_759 (A B C D : S) : (snot (snot (! A) D) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_760 (A B C D : S) : (sxor (snot (! A) D) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_761 (A B C D : S) : (sand (snot (! B) C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_762 (A B C D : S) : (sand (snot (! B) C) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_763 (A B C D : S) : (sand (snot (! B) C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_764 (A B C D : S) : (sand (snot (! B) C) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_765 (A B C D : S) : (sand (snot (! B) C) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_766 (A B C D : S) : (sand (snot (! B) C) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_767 (A B C D : S) : (sand (snot (! B) C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_768 (A B C D : S) : (sand (snot (! B) C) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_769 (A B C D : S) : (sand (snot (! B) C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_770 (A B C D : S) : (sand (snot (! B) C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_771 (A B C D : S) : (sand (snot (! B) C) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_772 (A B C D : S) : (sand (snot (! B) C) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_773 (A B C D : S) : (snot (snot (! B) C) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_774 (A B C D : S) : (snot (snot (! B) C) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_775 (A B C D : S) : (sxor (snot (! B) C) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_776 (A B C D : S) : (sand (snot (! B) D) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_777 (A B C D : S) : (sand (snot (! B) D) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_778 (A B C D : S) : (sand (snot (! B) D) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_779 (A B C D : S) : (sand (snot (! B) D) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_780 (A B C D : S) : (sand (snot (! B) D) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_781 (A B C D : S) : (sand (snot (! B) D) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_782 (A B C D : S) : (sand (snot (! B) D) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_783 (A B C D : S) : (sand (snot (! B) D) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_784 (A B C D : S) : (sand (snot (! B) D) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_785 (A B C D : S) : (sand (snot (! B) D) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_786 (A B C D : S) : (sand (snot (! B) D) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_787 (A B C D : S) : (sand (snot (! B) D) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_788 (A B C D : S) : (snot (snot (! B) D) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_789 (A B C D : S) : (snot (snot (! B) D) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_790 (A B C D : S) : (sxor (snot (! B) D) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_791 (A B C D : S) : (sand (snot (! C) D) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_792 (A B C D : S) : (sand (snot (! C) D) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_793 (A B C D : S) : (sand (snot (! C) D) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_794 (A B C D : S) : (sand (snot (! C) D) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_795 (A B C D : S) : (sand (snot (! C) D) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_796 (A B C D : S) : (sand (snot (! C) D) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_797 (A B C D : S) : (sand (snot (! C) D) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_798 (A B C D : S) : (sand (snot (! C) D) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_799 (A B C D : S) : (sand (snot (! C) D) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_800 (A B C D : S) : (sand (snot (! C) D) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_801 (A B C D : S) : (sand (snot (! C) D) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_802 (A B C D : S) : (sand (snot (! C) D) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_803 (A B C D : S) : (snot (snot (! C) D) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_804 (A B C D : S) : (snot (snot (! C) D) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_805 (A B C D : S) : (sxor (snot (! C) D) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_806 (A B C D : S) : (sand (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_807 (A B C D : S) : (sand (sand A (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_808 (A B C D : S) : (sand (sand A (! A)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_809 (A B C D : S) : (sand (sand A (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_810 (A B C D : S) : (sand (sand A (! A)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_811 (A B C D : S) : (sand (sand A (! A)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_812 (A B C D : S) : (sand (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_813 (A B C D : S) : (sand (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_814 (A B C D : S) : (sand (sand A (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_815 (A B C D : S) : (sand (sand A (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_816 (A B C D : S) : (sand (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_817 (A B C D : S) : (sand (sand A (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_818 (A B C D : S) : (sand (sand A (! A)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_819 (A B C D : S) : (sand (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_820 (A B C D : S) : (sand (sand A (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_821 (A B C D : S) : (sand (sand A (! A)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_822 (A B C D : S) : (sand (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_823 (A B C D : S) : (sand (sand A (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_824 (A B C D : S) : (sand (sand A (! A)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_825 (A B C D : S) : (sand (sand A (! A)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_826 (A B C D : S) : (sand (sand A (! A)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_827 (A B C D : S) : (sand (sand A (! A)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_828 (A B C D : S) : (sand (sand A (! A)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_829 (A B C D : S) : (sand (sand A (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_830 (A B C D : S) : (sand (sand A (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_831 (A B C D : S) : (sand (sand A (! A)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_832 (A B C D : S) : (sand (sand A (! A)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_833 (A B C D : S) : (sand (sand A (! A)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_834 (A B C D : S) : (sand (sand A (! A)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_835 (A B C D : S) : (sand (sand A (! A)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_836 (A B C D : S) : (sand (sand A (! A)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_837 (A B C D : S) : (snot (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_838 (A B C D : S) : (snot (sand A (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_839 (A B C D : S) : (snot (sand A (! A)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_840 (A B C D : S) : (snot (sand A (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_841 (A B C D : S) : (snot (sand A (! A)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_842 (A B C D : S) : (snot (sand A (! A)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_843 (A B C D : S) : (snot (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_844 (A B C D : S) : (snot (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_845 (A B C D : S) : (snot (sand A (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_846 (A B C D : S) : (snot (sand A (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_847 (A B C D : S) : (snot (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_848 (A B C D : S) : (snot (sand A (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_849 (A B C D : S) : (snot (sand A (! A)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_850 (A B C D : S) : (snot (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_851 (A B C D : S) : (snot (sand A (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_852 (A B C D : S) : (snot (sand A (! A)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_853 (A B C D : S) : (snot (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_854 (A B C D : S) : (snot (sand A (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_855 (A B C D : S) : (snot (sand A (! A)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_856 (A B C D : S) : (snot (sand A (! A)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_857 (A B C D : S) : (snot (sand A (! A)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_858 (A B C D : S) : (snot (sand A (! A)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_859 (A B C D : S) : (snot (sand A (! A)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_860 (A B C D : S) : (snot (sand A (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_861 (A B C D : S) : (snot (sand A (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_862 (A B C D : S) : (snot (sand A (! A)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_863 (A B C D : S) : (snot (sand A (! A)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_864 (A B C D : S) : (snot (sand A (! A)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_865 (A B C D : S) : (snot (sand A (! A)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_866 (A B C D : S) : (snot (sand A (! A)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_867 (A B C D : S) : (snot (sand A (! A)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_868 (A B C D : S) : (sxor (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_869 (A B C D : S) : (sand (sand A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_870 (A B C D : S) : (sand (sand A (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_871 (A B C D : S) : (sand (sand A (! B)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_872 (A B C D : S) : (sand (sand A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_873 (A B C D : S) : (sand (sand A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_874 (A B C D : S) : (sand (sand A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_875 (A B C D : S) : (sand (sand A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_876 (A B C D : S) : (sand (sand A (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_877 (A B C D : S) : (sand (sand A (! B)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_878 (A B C D : S) : (sand (sand A (! B)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_879 (A B C D : S) : (sand (sand A (! B)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_880 (A B C D : S) : (sand (sand A (! B)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_881 (A B C D : S) : (sand (sand A (! B)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_882 (A B C D : S) : (snot (sand A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_883 (A B C D : S) : (sxor (sand A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_884 (A B C D : S) : (sand (sand A (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_885 (A B C D : S) : (sand (sand A (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_886 (A B C D : S) : (sand (sand A (! C)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_887 (A B C D : S) : (sand (sand A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_888 (A B C D : S) : (sand (sand A (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_889 (A B C D : S) : (sand (sand A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_890 (A B C D : S) : (sand (sand A (! C)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_891 (A B C D : S) : (sand (sand A (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_892 (A B C D : S) : (sand (sand A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_893 (A B C D : S) : (sand (sand A (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_894 (A B C D : S) : (sand (sand A (! C)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_895 (A B C D : S) : (sand (sand A (! C)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_896 (A B C D : S) : (sand (sand A (! C)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_897 (A B C D : S) : (snot (sand A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_898 (A B C D : S) : (sxor (sand A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_899 (A B C D : S) : (sand (sand A (! D)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_900 (A B C D : S) : (sand (sand A (! D)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_901 (A B C D : S) : (sand (sand A (! D)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_902 (A B C D : S) : (sand (sand A (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_903 (A B C D : S) : (sand (sand A (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_904 (A B C D : S) : (sand (sand A (! D)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_905 (A B C D : S) : (sand (sand A (! D)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_906 (A B C D : S) : (sand (sand A (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_907 (A B C D : S) : (sand (sand A (! D)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_908 (A B C D : S) : (sand (sand A (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_909 (A B C D : S) : (sand (sand A (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_910 (A B C D : S) : (sand (sand A (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_911 (A B C D : S) : (sand (sand A (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_912 (A B C D : S) : (snot (sand A (! D)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_913 (A B C D : S) : (sxor (sand A (! D)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_914 (A B C D : S) : (sand (snot A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_915 (A B C D : S) : (sand (snot A (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_916 (A B C D : S) : (sand (snot A (! B)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_917 (A B C D : S) : (sand (snot A (! B)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_918 (A B C D : S) : (sand (snot A (! B)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_919 (A B C D : S) : (sand (snot A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_920 (A B C D : S) : (sand (snot A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_921 (A B C D : S) : (sand (snot A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_922 (A B C D : S) : (sand (snot A (! B)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_923 (A B C D : S) : (sand (snot A (! B)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_924 (A B C D : S) : (sand (snot A (! B)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_925 (A B C D : S) : (sand (snot A (! B)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_926 (A B C D : S) : (snot (snot A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_927 (A B C D : S) : (snot (snot A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_928 (A B C D : S) : (sxor (snot A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_929 (A B C D : S) : (sand (snot A (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_930 (A B C D : S) : (sand (snot A (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_931 (A B C D : S) : (sand (snot A (! C)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_932 (A B C D : S) : (sand (snot A (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_933 (A B C D : S) : (sand (snot A (! C)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_934 (A B C D : S) : (sand (snot A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_935 (A B C D : S) : (sand (snot A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_936 (A B C D : S) : (sand (snot A (! C)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_937 (A B C D : S) : (sand (snot A (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_938 (A B C D : S) : (sand (snot A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_939 (A B C D : S) : (sand (snot A (! C)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_940 (A B C D : S) : (sand (snot A (! C)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_941 (A B C D : S) : (snot (snot A (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_942 (A B C D : S) : (snot (snot A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_943 (A B C D : S) : (sxor (snot A (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_944 (A B C D : S) : (sand (snot A (! D)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_945 (A B C D : S) : (sand (snot A (! D)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_946 (A B C D : S) : (sand (snot A (! D)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_947 (A B C D : S) : (sand (snot A (! D)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_948 (A B C D : S) : (sand (snot A (! D)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_949 (A B C D : S) : (sand (snot A (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_950 (A B C D : S) : (sand (snot A (! D)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_951 (A B C D : S) : (sand (snot A (! D)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_952 (A B C D : S) : (sand (snot A (! D)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_953 (A B C D : S) : (sand (snot A (! D)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_954 (A B C D : S) : (sand (snot A (! D)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_955 (A B C D : S) : (sand (snot A (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_956 (A B C D : S) : (snot (snot A (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_957 (A B C D : S) : (snot (snot A (! D)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_958 (A B C D : S) : (sxor (snot A (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_959 (A B C D : S) : (sand (sxor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_960 (A B C D : S) : (sand (sxor A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_961 (A B C D : S) : (sand (sxor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_962 (A B C D : S) : (snot (sxor A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_963 (A B C D : S) : (sxor (sxor A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_964 (A B C D : S) : (sand (sxor A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_965 (A B C D : S) : (sand (sxor A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_966 (A B C D : S) : (sand (sxor A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_967 (A B C D : S) : (snot (sxor A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_968 (A B C D : S) : (sxor (sxor A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_969 (A B C D : S) : (sand (sxor A (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_970 (A B C D : S) : (sand (sxor A (! D)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_971 (A B C D : S) : (sand (sxor A (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_972 (A B C D : S) : (snot (sxor A (! D)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_973 (A B C D : S) : (sxor (sxor A (! D)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_974 (A B C D : S) : (sand (sand B (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_975 (A B C D : S) : (sand (sand B (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_976 (A B C D : S) : (sand (sand B (! A)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_977 (A B C D : S) : (sand (sand B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_978 (A B C D : S) : (sand (sand B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_979 (A B C D : S) : (sand (sand B (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_980 (A B C D : S) : (sand (sand B (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_981 (A B C D : S) : (sand (sand B (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_982 (A B C D : S) : (sand (sand B (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_983 (A B C D : S) : (sand (sand B (! A)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_984 (A B C D : S) : (sand (sand B (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_985 (A B C D : S) : (sand (sand B (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_986 (A B C D : S) : (sand (sand B (! A)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_987 (A B C D : S) : (snot (sand B (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_988 (A B C D : S) : (sxor (sand B (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_989 (A B C D : S) : (sand (sand B (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_990 (A B C D : S) : (sand (sand B (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_991 (A B C D : S) : (sand (sand B (! C)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_992 (A B C D : S) : (sand (sand B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_993 (A B C D : S) : (sand (sand B (! C)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_994 (A B C D : S) : (sand (sand B (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_995 (A B C D : S) : (sand (sand B (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_996 (A B C D : S) : (sand (sand B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_997 (A B C D : S) : (sand (sand B (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_998 (A B C D : S) : (sand (sand B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_999 (A B C D : S) : (sand (sand B (! C)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1000 (A B C D : S) : (sand (sand B (! C)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1001 (A B C D : S) : (sand (sand B (! C)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1002 (A B C D : S) : (snot (sand B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1003 (A B C D : S) : (sxor (sand B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1004 (A B C D : S) : (sand (sand B (! D)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1005 (A B C D : S) : (sand (sand B (! D)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1006 (A B C D : S) : (sand (sand B (! D)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1007 (A B C D : S) : (sand (sand B (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1008 (A B C D : S) : (sand (sand B (! D)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1009 (A B C D : S) : (sand (sand B (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1010 (A B C D : S) : (sand (sand B (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1011 (A B C D : S) : (sand (sand B (! D)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1012 (A B C D : S) : (sand (sand B (! D)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1013 (A B C D : S) : (sand (sand B (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1014 (A B C D : S) : (sand (sand B (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1015 (A B C D : S) : (sand (sand B (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1016 (A B C D : S) : (sand (sand B (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1017 (A B C D : S) : (snot (sand B (! D)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1018 (A B C D : S) : (sxor (sand B (! D)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1019 (A B C D : S) : (sand (snot B (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1020 (A B C D : S) : (sand (snot B (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1021 (A B C D : S) : (sand (snot B (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1022 (A B C D : S) : (sand (snot B (! C)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1023 (A B C D : S) : (sand (snot B (! C)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1024 (A B C D : S) : (sand (snot B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1025 (A B C D : S) : (sand (snot B (! C)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1026 (A B C D : S) : (sand (snot B (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1027 (A B C D : S) : (sand (snot B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1028 (A B C D : S) : (sand (snot B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1029 (A B C D : S) : (sand (snot B (! C)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1030 (A B C D : S) : (sand (snot B (! C)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1031 (A B C D : S) : (snot (snot B (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1032 (A B C D : S) : (snot (snot B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1033 (A B C D : S) : (sxor (snot B (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1034 (A B C D : S) : (sand (snot B (! D)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1035 (A B C D : S) : (sand (snot B (! D)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1036 (A B C D : S) : (sand (snot B (! D)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1037 (A B C D : S) : (sand (snot B (! D)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1038 (A B C D : S) : (sand (snot B (! D)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1039 (A B C D : S) : (sand (snot B (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1040 (A B C D : S) : (sand (snot B (! D)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1041 (A B C D : S) : (sand (snot B (! D)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1042 (A B C D : S) : (sand (snot B (! D)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1043 (A B C D : S) : (sand (snot B (! D)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1044 (A B C D : S) : (sand (snot B (! D)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1045 (A B C D : S) : (sand (snot B (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1046 (A B C D : S) : (snot (snot B (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1047 (A B C D : S) : (snot (snot B (! D)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1048 (A B C D : S) : (sxor (snot B (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1049 (A B C D : S) : (sand (sxor B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1050 (A B C D : S) : (sand (sxor B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1051 (A B C D : S) : (sand (sxor B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1052 (A B C D : S) : (snot (sxor B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1053 (A B C D : S) : (sxor (sxor B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1054 (A B C D : S) : (sand (sxor B (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1055 (A B C D : S) : (sand (sxor B (! D)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1056 (A B C D : S) : (sand (sxor B (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1057 (A B C D : S) : (snot (sxor B (! D)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1058 (A B C D : S) : (sxor (sxor B (! D)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1059 (A B C D : S) : (sand (sand C (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1060 (A B C D : S) : (sand (sand C (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1061 (A B C D : S) : (sand (sand C (! A)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1062 (A B C D : S) : (sand (sand C (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1063 (A B C D : S) : (sand (sand C (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1064 (A B C D : S) : (sand (sand C (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1065 (A B C D : S) : (sand (sand C (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1066 (A B C D : S) : (sand (sand C (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1067 (A B C D : S) : (sand (sand C (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1068 (A B C D : S) : (sand (sand C (! A)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1069 (A B C D : S) : (sand (sand C (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1070 (A B C D : S) : (sand (sand C (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1071 (A B C D : S) : (sand (sand C (! A)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1072 (A B C D : S) : (snot (sand C (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1073 (A B C D : S) : (sxor (sand C (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1074 (A B C D : S) : (sand (sand C (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1075 (A B C D : S) : (sand (sand C (! B)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1076 (A B C D : S) : (sand (sand C (! B)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1077 (A B C D : S) : (sand (sand C (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1078 (A B C D : S) : (sand (sand C (! B)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1079 (A B C D : S) : (sand (sand C (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1080 (A B C D : S) : (sand (sand C (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1081 (A B C D : S) : (sand (sand C (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1082 (A B C D : S) : (sand (sand C (! B)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1083 (A B C D : S) : (sand (sand C (! B)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1084 (A B C D : S) : (sand (sand C (! B)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1085 (A B C D : S) : (sand (sand C (! B)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1086 (A B C D : S) : (sand (sand C (! B)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1087 (A B C D : S) : (snot (sand C (! B)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1088 (A B C D : S) : (sxor (sand C (! B)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1089 (A B C D : S) : (sand (sand C (! D)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1090 (A B C D : S) : (sand (sand C (! D)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1091 (A B C D : S) : (sand (sand C (! D)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1092 (A B C D : S) : (sand (sand C (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1093 (A B C D : S) : (sand (sand C (! D)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1094 (A B C D : S) : (sand (sand C (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1095 (A B C D : S) : (sand (sand C (! D)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1096 (A B C D : S) : (sand (sand C (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1097 (A B C D : S) : (sand (sand C (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1098 (A B C D : S) : (sand (sand C (! D)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1099 (A B C D : S) : (sand (sand C (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1100 (A B C D : S) : (sand (sand C (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1101 (A B C D : S) : (sand (sand C (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1102 (A B C D : S) : (snot (sand C (! D)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1103 (A B C D : S) : (sxor (sand C (! D)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1104 (A B C D : S) : (sand (snot C (! D)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1105 (A B C D : S) : (sand (snot C (! D)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1106 (A B C D : S) : (sand (snot C (! D)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1107 (A B C D : S) : (sand (snot C (! D)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1108 (A B C D : S) : (sand (snot C (! D)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1109 (A B C D : S) : (sand (snot C (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1110 (A B C D : S) : (sand (snot C (! D)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1111 (A B C D : S) : (sand (snot C (! D)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1112 (A B C D : S) : (sand (snot C (! D)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1113 (A B C D : S) : (sand (snot C (! D)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1114 (A B C D : S) : (sand (snot C (! D)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1115 (A B C D : S) : (sand (snot C (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1116 (A B C D : S) : (snot (snot C (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1117 (A B C D : S) : (snot (snot C (! D)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1118 (A B C D : S) : (sxor (snot C (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1119 (A B C D : S) : (sand (sxor C (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1120 (A B C D : S) : (sand (sxor C (! D)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1121 (A B C D : S) : (sand (sxor C (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1122 (A B C D : S) : (snot (sxor C (! D)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1123 (A B C D : S) : (sxor (sxor C (! D)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1124 (A B C D : S) : (sand (sand D (! A)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1125 (A B C D : S) : (sand (sand D (! A)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1126 (A B C D : S) : (sand (sand D (! A)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1127 (A B C D : S) : (sand (sand D (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1128 (A B C D : S) : (sand (sand D (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1129 (A B C D : S) : (sand (sand D (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1130 (A B C D : S) : (sand (sand D (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1131 (A B C D : S) : (sand (sand D (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1132 (A B C D : S) : (sand (sand D (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1133 (A B C D : S) : (sand (sand D (! A)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1134 (A B C D : S) : (sand (sand D (! A)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1135 (A B C D : S) : (sand (sand D (! A)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1136 (A B C D : S) : (sand (sand D (! A)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1137 (A B C D : S) : (snot (sand D (! A)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1138 (A B C D : S) : (sxor (sand D (! A)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1139 (A B C D : S) : (sand (sand D (! B)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1140 (A B C D : S) : (sand (sand D (! B)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1141 (A B C D : S) : (sand (sand D (! B)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1142 (A B C D : S) : (sand (sand D (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1143 (A B C D : S) : (sand (sand D (! B)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1144 (A B C D : S) : (sand (sand D (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1145 (A B C D : S) : (sand (sand D (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1146 (A B C D : S) : (sand (sand D (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1147 (A B C D : S) : (sand (sand D (! B)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1148 (A B C D : S) : (sand (sand D (! B)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1149 (A B C D : S) : (sand (sand D (! B)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1150 (A B C D : S) : (sand (sand D (! B)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1151 (A B C D : S) : (sand (sand D (! B)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1152 (A B C D : S) : (snot (sand D (! B)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1153 (A B C D : S) : (sxor (sand D (! B)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1154 (A B C D : S) : (sand (sand D (! C)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1155 (A B C D : S) : (sand (sand D (! C)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1156 (A B C D : S) : (sand (sand D (! C)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1157 (A B C D : S) : (sand (sand D (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1158 (A B C D : S) : (sand (sand D (! C)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1159 (A B C D : S) : (sand (sand D (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1160 (A B C D : S) : (sand (sand D (! C)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1161 (A B C D : S) : (sand (sand D (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1162 (A B C D : S) : (sand (sand D (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1163 (A B C D : S) : (sand (sand D (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1164 (A B C D : S) : (sand (sand D (! C)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1165 (A B C D : S) : (sand (sand D (! C)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1166 (A B C D : S) : (sand (sand D (! C)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1167 (A B C D : S) : (snot (sand D (! C)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1168 (A B C D : S) : (sxor (sand D (! C)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1169 (A B C D : S) : (soutside (! A) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1170 (A B C D : S) : (soutside (! A) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1171 (A B C D : S) : (soutside (! A) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1172 (A B C D : S) : (soutside (! B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1173 (A B C D : S) : (soutside (! B) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1174 (A B C D : S) : (soutside (! B) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1175 (A B C D : S) : (soutside (! C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1176 (A B C D : S) : (soutside (! C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1177 (A B C D : S) : (soutside (! C) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1178 (A B C D : S) : (soutside (! D) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1179 (A B C D : S) : (soutside (! D) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1180 (A B C D : S) : (soutside (! D) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1181 (A B C D : S) : (sinside (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1182 (A B C D : S) : (sinside (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1183 (A B C D : S) : (sinside (sand A (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1184 (A B C D : S) : (sinside (sand A (! A)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1185 (A B C D : S) : (sinside (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1186 (A B C D : S) : (sinside (sand A (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1187 (A B C D : S) : (sinside (sand A (! A)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1188 (A B C D : S) : (sinside (sand A (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1189 (A B C D : S) : (sinside (sand A (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1190 (A B C D : S) : (sinside (sand A (! A)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1191 (A B C D : S) : (sinside (sand A (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1192 (A B C D : S) : (sinside (sand A (! A)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1193 (A B C D : S) : (sinside (sand A (! A)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1194 (A B C D : S) : (sinside (sand A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1195 (A B C D : S) : (sinside (sand A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1196 (A B C D : S) : (sinside (sand A (! D)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1197 (A B C D : S) : (sinside (sor A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1198 (A B C D : S) : (soutside (sor A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1199 (A B C D : S) : (sinside (sor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1200 (A B C D : S) : (sinside (sor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1201 (A B C D : S) : (soutside (sor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1202 (A B C D : S) : (soutside (sor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1203 (A B C D : S) : (sinside (sor A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1204 (A B C D : S) : (sinside (sor A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1205 (A B C D : S) : (soutside (sor A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1206 (A B C D : S) : (soutside (sor A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1207 (A B C D : S) : (sinside (sor A (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1208 (A B C D : S) : (sinside (sor A (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1209 (A B C D : S) : (soutside (sor A (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1210 (A B C D : S) : (soutside (sor A (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1211 (A B C D : S) : (sinside (sand B (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1212 (A B C D : S) : (sinside (sand B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1213 (A B C D : S) : (sinside (sand B (! D)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1214 (A B C D : S) : (sinside (sor B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1215 (A B C D : S) : (sinside (sor B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1216 (A B C D : S) : (soutside (sor B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1217 (A B C D : S) : (soutside (sor B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1218 (A B C D : S) : (sinside (sor B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1219 (A B C D : S) : (sinside (sor B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1220 (A B C D : S) : (soutside (sor B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1221 (A B C D : S) : (soutside (sor B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1222 (A B C D : S) : (sinside (sor B (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1223 (A B C D : S) : (sinside (sor B (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1224 (A B C D : S) : (soutside (sor B (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1225 (A B C D : S) : (soutside (sor B (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1226 (A B C D : S) : (sinside (sand C (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1227 (A B C D : S) : (sinside (sand C (! B)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1228 (A B C D : S) : (sinside (sand C (! D)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1229 (A B C D : S) : (sinside (sor C (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1230 (A B C D : S) : (sinside (sor C (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1231 (A B C D : S) : (soutside (sor C (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1232 (A B C D : S) : (soutside (sor C (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1233 (A B C D : S) : (sinside (sor C (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1234 (A B C D : S) : (sinside (sor C (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1235 (A B C D : S) : (soutside (sor C (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1236 (A B C D : S) : (soutside (sor C (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1237 (A B C D : S) : (sinside (sor C (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1238 (A B C D : S) : (sinside (sor C (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1239 (A B C D : S) : (soutside (sor C (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1240 (A B C D : S) : (soutside (sor C (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1241 (A B C D : S) : (sinside (sand D (! A)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1242 (A B C D : S) : (sinside (sand D (! B)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1243 (A B C D : S) : (sinside (sand D (! C)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1244 (A B C D : S) : (sinside (sor D (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1245 (A B C D : S) : (sinside (sor D (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1246 (A B C D : S) : (soutside (sor D (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1247 (A B C D : S) : (soutside (sor D (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1248 (A B C D : S) : (sinside (sor D (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1249 (A B C D : S) : (sinside (sor D (! B)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1250 (A B C D : S) : (soutside (sor D (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1251 (A B C D : S) : (soutside (sor D (! B)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1252 (A B C D : S) : (sinside (sor D (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1253 (A B C D : S) : (sinside (sor D (! C)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1254 (A B C D : S) : (soutside (sor D (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1255 (A B C D : S) : (soutside (sor D (! C)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1256 (A B C D : S) : (sinside (snot (! A) B) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1257 (A B C D : S) : (sinside (snot (! A) B) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1258 (A B C D : S) : (sinside (snot (! A) B) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1259 (A B C D : S) : (sinside (snot (! A) B) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1260 (A B C D : S) : (sinside (snot (! A) B) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1261 (A B C D : S) : (sinside (snot (! A) B) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1262 (A B C D : S) : (sinside (snot (! A) B) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1263 (A B C D : S) : (sinside (snot (! A) B) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1264 (A B C D : S) : (sinside (snot (! A) B) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1265 (A B C D : S) : (sinside (snot (! A) B) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1266 (A B C D : S) : (sinside (snot (! A) B) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1267 (A B C D : S) : (sinside (snot (! A) B) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1268 (A B C D : S) : (soutside (snot (! A) B) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1269 (A B C D : S) : (soutside (snot (! A) B) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1270 (A B C D : S) : (soutside (snot (! A) B) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1271 (A B C D : S) : (soutside (snot (! A) B) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1272 (A B C D : S) : (soutside (snot (! A) B) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1273 (A B C D : S) : (soutside (snot (! A) B) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1274 (A B C D : S) : (soutside (snot (! A) B) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1275 (A B C D : S) : (sinside (snot (! A) C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1276 (A B C D : S) : (sinside (snot (! A) C) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1277 (A B C D : S) : (sinside (snot (! A) C) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1278 (A B C D : S) : (sinside (snot (! A) C) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1279 (A B C D : S) : (sinside (snot (! A) C) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1280 (A B C D : S) : (sinside (snot (! A) C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1281 (A B C D : S) : (sinside (snot (! A) C) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1282 (A B C D : S) : (sinside (snot (! A) C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1283 (A B C D : S) : (sinside (snot (! A) C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1284 (A B C D : S) : (sinside (snot (! A) C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1285 (A B C D : S) : (sinside (snot (! A) C) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1286 (A B C D : S) : (sinside (snot (! A) C) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1287 (A B C D : S) : (soutside (snot (! A) C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1288 (A B C D : S) : (soutside (snot (! A) C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1289 (A B C D : S) : (soutside (snot (! A) C) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1290 (A B C D : S) : (soutside (snot (! A) C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1291 (A B C D : S) : (soutside (snot (! A) C) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1292 (A B C D : S) : (soutside (snot (! A) C) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1293 (A B C D : S) : (soutside (snot (! A) C) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1294 (A B C D : S) : (sinside (snot (! A) D) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1295 (A B C D : S) : (sinside (snot (! A) D) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1296 (A B C D : S) : (sinside (snot (! A) D) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1297 (A B C D : S) : (sinside (snot (! A) D) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1298 (A B C D : S) : (sinside (snot (! A) D) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1299 (A B C D : S) : (sinside (snot (! A) D) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1300 (A B C D : S) : (sinside (snot (! A) D) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1301 (A B C D : S) : (sinside (snot (! A) D) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1302 (A B C D : S) : (sinside (snot (! A) D) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1303 (A B C D : S) : (sinside (snot (! A) D) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1304 (A B C D : S) : (sinside (snot (! A) D) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1305 (A B C D : S) : (sinside (snot (! A) D) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1306 (A B C D : S) : (soutside (snot (! A) D) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1307 (A B C D : S) : (soutside (snot (! A) D) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1308 (A B C D : S) : (soutside (snot (! A) D) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1309 (A B C D : S) : (soutside (snot (! A) D) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1310 (A B C D : S) : (soutside (snot (! A) D) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1311 (A B C D : S) : (soutside (snot (! A) D) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1312 (A B C D : S) : (soutside (snot (! A) D) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1313 (A B C D : S) : (sinside (snot (! B) C) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1314 (A B C D : S) : (sinside (snot (! B) C) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1315 (A B C D : S) : (sinside (snot (! B) C) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1316 (A B C D : S) : (sinside (snot (! B) C) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1317 (A B C D : S) : (sinside (snot (! B) C) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1318 (A B C D : S) : (sinside (snot (! B) C) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1319 (A B C D : S) : (sinside (snot (! B) C) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1320 (A B C D : S) : (sinside (snot (! B) C) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1321 (A B C D : S) : (sinside (snot (! B) C) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1322 (A B C D : S) : (sinside (snot (! B) C) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1323 (A B C D : S) : (sinside (snot (! B) C) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1324 (A B C D : S) : (sinside (snot (! B) C) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1325 (A B C D : S) : (soutside (snot (! B) C) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1326 (A B C D : S) : (soutside (snot (! B) C) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1327 (A B C D : S) : (soutside (snot (! B) C) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1328 (A B C D : S) : (soutside (snot (! B) C) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1329 (A B C D : S) : (soutside (snot (! B) C) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1330 (A B C D : S) : (soutside (snot (! B) C) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1331 (A B C D : S) : (soutside (snot (! B) C) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1332 (A B C D : S) : (sinside (snot (! B) D) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1333 (A B C D : S) : (sinside (snot (! B) D) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1334 (A B C D : S) : (sinside (snot (! B) D) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1335 (A B C D : S) : (sinside (snot (! B) D) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1336 (A B C D : S) : (sinside (snot (! B) D) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1337 (A B C D : S) : (sinside (snot (! B) D) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1338 (A B C D : S) : (sinside (snot (! B) D) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1339 (A B C D : S) : (sinside (snot (! B) D) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1340 (A B C D : S) : (sinside (snot (! B) D) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1341 (A B C D : S) : (sinside (snot (! B) D) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1342 (A B C D : S) : (sinside (snot (! B) D) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1343 (A B C D : S) : (sinside (snot (! B) D) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1344 (A B C D : S) : (soutside (snot (! B) D) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1345 (A B C D : S) : (soutside (snot (! B) D) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1346 (A B C D : S) : (soutside (snot (! B) D) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1347 (A B C D : S) : (soutside (snot (! B) D) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1348 (A B C D : S) : (soutside (snot (! B) D) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1349 (A B C D : S) : (soutside (snot (! B) D) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1350 (A B C D : S) : (soutside (snot (! B) D) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1351 (A B C D : S) : (sinside (snot (! C) D) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1352 (A B C D : S) : (sinside (snot (! C) D) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1353 (A B C D : S) : (sinside (snot (! C) D) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1354 (A B C D : S) : (sinside (snot (! C) D) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1355 (A B C D : S) : (sinside (snot (! C) D) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1356 (A B C D : S) : (sinside (snot (! C) D) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1357 (A B C D : S) : (sinside (snot (! C) D) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1358 (A B C D : S) : (sinside (snot (! C) D) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1359 (A B C D : S) : (sinside (snot (! C) D) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1360 (A B C D : S) : (sinside (snot (! C) D) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1361 (A B C D : S) : (sinside (snot (! C) D) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1362 (A B C D : S) : (sinside (snot (! C) D) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1363 (A B C D : S) : (soutside (snot (! C) D) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1364 (A B C D : S) : (soutside (snot (! C) D) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1365 (A B C D : S) : (soutside (snot (! C) D) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1366 (A B C D : S) : (soutside (snot (! C) D) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1367 (A B C D : S) : (soutside (snot (! C) D) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1368 (A B C D : S) : (soutside (snot (! C) D) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1369 (A B C D : S) : (soutside (snot (! C) D) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1370 (A B C D : S) : (sinside (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1371 (A B C D : S) : (sinside (sand A (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1372 (A B C D : S) : (sinside (sand A (! A)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1373 (A B C D : S) : (sinside (sand A (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1374 (A B C D : S) : (sinside (sand A (! A)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1375 (A B C D : S) : (sinside (sand A (! A)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1376 (A B C D : S) : (sinside (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1377 (A B C D : S) : (sinside (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1378 (A B C D : S) : (sinside (sand A (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1379 (A B C D : S) : (sinside (sand A (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1380 (A B C D : S) : (sinside (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1381 (A B C D : S) : (sinside (sand A (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1382 (A B C D : S) : (sinside (sand A (! A)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1383 (A B C D : S) : (sinside (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1384 (A B C D : S) : (sinside (sand A (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1385 (A B C D : S) : (sinside (sand A (! A)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1386 (A B C D : S) : (sinside (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1387 (A B C D : S) : (sinside (sand A (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1388 (A B C D : S) : (sinside (sand A (! A)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1389 (A B C D : S) : (sinside (sand A (! A)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1390 (A B C D : S) : (sinside (sand A (! A)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1391 (A B C D : S) : (sinside (sand A (! A)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1392 (A B C D : S) : (sinside (sand A (! A)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1393 (A B C D : S) : (sinside (sand A (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1394 (A B C D : S) : (sinside (sand A (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1395 (A B C D : S) : (sinside (sand A (! A)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1396 (A B C D : S) : (sinside (sand A (! A)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1397 (A B C D : S) : (sinside (sand A (! A)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1398 (A B C D : S) : (sinside (sand A (! A)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1399 (A B C D : S) : (sinside (sand A (! A)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1400 (A B C D : S) : (sinside (sand A (! A)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1401 (A B C D : S) : (soutside (sand A (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1402 (A B C D : S) : (soutside (sand A (! A)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1403 (A B C D : S) : (soutside (sand A (! A)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1404 (A B C D : S) : (soutside (sand A (! A)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1405 (A B C D : S) : (soutside (sand A (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1406 (A B C D : S) : (soutside (sand A (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1407 (A B C D : S) : (soutside (sand A (! A)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1408 (A B C D : S) : (soutside (sand A (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1409 (A B C D : S) : (soutside (sand A (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1410 (A B C D : S) : (soutside (sand A (! A)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1411 (A B C D : S) : (soutside (sand A (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1412 (A B C D : S) : (soutside (sand A (! A)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1413 (A B C D : S) : (soutside (sand A (! A)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1414 (A B C D : S) : (sinside (sand A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1415 (A B C D : S) : (sinside (sand A (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1416 (A B C D : S) : (sinside (sand A (! B)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1417 (A B C D : S) : (sinside (sand A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1418 (A B C D : S) : (sinside (sand A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1419 (A B C D : S) : (sinside (sand A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1420 (A B C D : S) : (sinside (sand A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1421 (A B C D : S) : (sinside (sand A (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1422 (A B C D : S) : (sinside (sand A (! B)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1423 (A B C D : S) : (sinside (sand A (! B)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1424 (A B C D : S) : (sinside (sand A (! B)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1425 (A B C D : S) : (sinside (sand A (! B)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1426 (A B C D : S) : (sinside (sand A (! B)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1427 (A B C D : S) : (soutside (sand A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1428 (A B C D : S) : (soutside (sand A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1429 (A B C D : S) : (soutside (sand A (! B)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1430 (A B C D : S) : (soutside (sand A (! B)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1431 (A B C D : S) : (soutside (sand A (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1432 (A B C D : S) : (soutside (sand A (! B)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1433 (A B C D : S) : (sinside (sand A (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1434 (A B C D : S) : (sinside (sand A (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1435 (A B C D : S) : (sinside (sand A (! C)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1436 (A B C D : S) : (sinside (sand A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1437 (A B C D : S) : (sinside (sand A (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1438 (A B C D : S) : (sinside (sand A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1439 (A B C D : S) : (sinside (sand A (! C)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1440 (A B C D : S) : (sinside (sand A (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1441 (A B C D : S) : (sinside (sand A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1442 (A B C D : S) : (sinside (sand A (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1443 (A B C D : S) : (sinside (sand A (! C)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1444 (A B C D : S) : (sinside (sand A (! C)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1445 (A B C D : S) : (sinside (sand A (! C)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1446 (A B C D : S) : (soutside (sand A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1447 (A B C D : S) : (soutside (sand A (! C)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1448 (A B C D : S) : (soutside (sand A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1449 (A B C D : S) : (soutside (sand A (! C)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1450 (A B C D : S) : (soutside (sand A (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1451 (A B C D : S) : (soutside (sand A (! C)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1452 (A B C D : S) : (sinside (sand A (! D)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1453 (A B C D : S) : (sinside (sand A (! D)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1454 (A B C D : S) : (sinside (sand A (! D)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1455 (A B C D : S) : (sinside (sand A (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1456 (A B C D : S) : (sinside (sand A (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1457 (A B C D : S) : (sinside (sand A (! D)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1458 (A B C D : S) : (sinside (sand A (! D)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1459 (A B C D : S) : (sinside (sand A (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1460 (A B C D : S) : (sinside (sand A (! D)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1461 (A B C D : S) : (sinside (sand A (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1462 (A B C D : S) : (sinside (sand A (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1463 (A B C D : S) : (sinside (sand A (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1464 (A B C D : S) : (sinside (sand A (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1465 (A B C D : S) : (soutside (sand A (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1466 (A B C D : S) : (soutside (sand A (! D)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1467 (A B C D : S) : (soutside (sand A (! D)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1468 (A B C D : S) : (soutside (sand A (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1469 (A B C D : S) : (soutside (sand A (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1470 (A B C D : S) : (soutside (sand A (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1471 (A B C D : S) : (sinside (snot A (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1472 (A B C D : S) : (sinside (snot A (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1473 (A B C D : S) : (sinside (snot A (! B)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1474 (A B C D : S) : (sinside (snot A (! B)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1475 (A B C D : S) : (sinside (snot A (! B)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1476 (A B C D : S) : (sinside (snot A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1477 (A B C D : S) : (sinside (snot A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1478 (A B C D : S) : (sinside (snot A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1479 (A B C D : S) : (sinside (snot A (! B)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1480 (A B C D : S) : (sinside (snot A (! B)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1481 (A B C D : S) : (sinside (snot A (! B)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1482 (A B C D : S) : (sinside (snot A (! B)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1483 (A B C D : S) : (soutside (snot A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1484 (A B C D : S) : (soutside (snot A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1485 (A B C D : S) : (soutside (snot A (! B)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1486 (A B C D : S) : (soutside (snot A (! B)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1487 (A B C D : S) : (soutside (snot A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1488 (A B C D : S) : (soutside (snot A (! B)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1489 (A B C D : S) : (soutside (snot A (! B)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1490 (A B C D : S) : (sinside (snot A (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1491 (A B C D : S) : (sinside (snot A (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1492 (A B C D : S) : (sinside (snot A (! C)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1493 (A B C D : S) : (sinside (snot A (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1494 (A B C D : S) : (sinside (snot A (! C)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1495 (A B C D : S) : (sinside (snot A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1496 (A B C D : S) : (sinside (snot A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1497 (A B C D : S) : (sinside (snot A (! C)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1498 (A B C D : S) : (sinside (snot A (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1499 (A B C D : S) : (sinside (snot A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1500 (A B C D : S) : (sinside (snot A (! C)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1501 (A B C D : S) : (sinside (snot A (! C)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1502 (A B C D : S) : (soutside (snot A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1503 (A B C D : S) : (soutside (snot A (! C)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1504 (A B C D : S) : (soutside (snot A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1505 (A B C D : S) : (soutside (snot A (! C)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1506 (A B C D : S) : (soutside (snot A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1507 (A B C D : S) : (soutside (snot A (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1508 (A B C D : S) : (soutside (snot A (! C)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1509 (A B C D : S) : (sinside (snot A (! D)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1510 (A B C D : S) : (sinside (snot A (! D)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1511 (A B C D : S) : (sinside (snot A (! D)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1512 (A B C D : S) : (sinside (snot A (! D)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1513 (A B C D : S) : (sinside (snot A (! D)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1514 (A B C D : S) : (sinside (snot A (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1515 (A B C D : S) : (sinside (snot A (! D)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1516 (A B C D : S) : (sinside (snot A (! D)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1517 (A B C D : S) : (sinside (snot A (! D)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1518 (A B C D : S) : (sinside (snot A (! D)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1519 (A B C D : S) : (sinside (snot A (! D)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1520 (A B C D : S) : (sinside (snot A (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1521 (A B C D : S) : (soutside (snot A (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1522 (A B C D : S) : (soutside (snot A (! D)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1523 (A B C D : S) : (soutside (snot A (! D)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1524 (A B C D : S) : (soutside (snot A (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1525 (A B C D : S) : (soutside (snot A (! D)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1526 (A B C D : S) : (soutside (snot A (! D)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1527 (A B C D : S) : (soutside (snot A (! D)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1528 (A B C D : S) : (sinside (sxor A (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1529 (A B C D : S) : (sinside (sxor A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1530 (A B C D : S) : (sinside (sxor A (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1531 (A B C D : S) : (soutside (sxor A (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1532 (A B C D : S) : (soutside (sxor A (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1533 (A B C D : S) : (soutside (sxor A (! B)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1534 (A B C D : S) : (sinside (sxor A (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1535 (A B C D : S) : (sinside (sxor A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1536 (A B C D : S) : (sinside (sxor A (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1537 (A B C D : S) : (soutside (sxor A (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1538 (A B C D : S) : (soutside (sxor A (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1539 (A B C D : S) : (soutside (sxor A (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1540 (A B C D : S) : (sinside (sxor A (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1541 (A B C D : S) : (sinside (sxor A (! D)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1542 (A B C D : S) : (sinside (sxor A (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1543 (A B C D : S) : (soutside (sxor A (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1544 (A B C D : S) : (soutside (sxor A (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1545 (A B C D : S) : (soutside (sxor A (! D)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1546 (A B C D : S) : (sinside (sand B (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1547 (A B C D : S) : (sinside (sand B (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1548 (A B C D : S) : (sinside (sand B (! A)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1549 (A B C D : S) : (sinside (sand B (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1550 (A B C D : S) : (sinside (sand B (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1551 (A B C D : S) : (sinside (sand B (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1552 (A B C D : S) : (sinside (sand B (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1553 (A B C D : S) : (sinside (sand B (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1554 (A B C D : S) : (sinside (sand B (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1555 (A B C D : S) : (sinside (sand B (! A)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1556 (A B C D : S) : (sinside (sand B (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1557 (A B C D : S) : (sinside (sand B (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1558 (A B C D : S) : (sinside (sand B (! A)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1559 (A B C D : S) : (soutside (sand B (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1560 (A B C D : S) : (soutside (sand B (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1561 (A B C D : S) : (soutside (sand B (! A)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1562 (A B C D : S) : (soutside (sand B (! A)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1563 (A B C D : S) : (soutside (sand B (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1564 (A B C D : S) : (soutside (sand B (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1565 (A B C D : S) : (sinside (sand B (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1566 (A B C D : S) : (sinside (sand B (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1567 (A B C D : S) : (sinside (sand B (! C)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1568 (A B C D : S) : (sinside (sand B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1569 (A B C D : S) : (sinside (sand B (! C)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1570 (A B C D : S) : (sinside (sand B (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1571 (A B C D : S) : (sinside (sand B (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1572 (A B C D : S) : (sinside (sand B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1573 (A B C D : S) : (sinside (sand B (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1574 (A B C D : S) : (sinside (sand B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1575 (A B C D : S) : (sinside (sand B (! C)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1576 (A B C D : S) : (sinside (sand B (! C)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1577 (A B C D : S) : (sinside (sand B (! C)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1578 (A B C D : S) : (soutside (sand B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1579 (A B C D : S) : (soutside (sand B (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1580 (A B C D : S) : (soutside (sand B (! C)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1581 (A B C D : S) : (soutside (sand B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1582 (A B C D : S) : (soutside (sand B (! C)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1583 (A B C D : S) : (soutside (sand B (! C)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1584 (A B C D : S) : (sinside (sand B (! D)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1585 (A B C D : S) : (sinside (sand B (! D)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1586 (A B C D : S) : (sinside (sand B (! D)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1587 (A B C D : S) : (sinside (sand B (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1588 (A B C D : S) : (sinside (sand B (! D)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1589 (A B C D : S) : (sinside (sand B (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1590 (A B C D : S) : (sinside (sand B (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1591 (A B C D : S) : (sinside (sand B (! D)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1592 (A B C D : S) : (sinside (sand B (! D)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1593 (A B C D : S) : (sinside (sand B (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1594 (A B C D : S) : (sinside (sand B (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1595 (A B C D : S) : (sinside (sand B (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1596 (A B C D : S) : (sinside (sand B (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1597 (A B C D : S) : (soutside (sand B (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1598 (A B C D : S) : (soutside (sand B (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1599 (A B C D : S) : (soutside (sand B (! D)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1600 (A B C D : S) : (soutside (sand B (! D)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1601 (A B C D : S) : (soutside (sand B (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1602 (A B C D : S) : (soutside (sand B (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1603 (A B C D : S) : (sinside (snot B (! C)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1604 (A B C D : S) : (sinside (snot B (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1605 (A B C D : S) : (sinside (snot B (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1606 (A B C D : S) : (sinside (snot B (! C)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1607 (A B C D : S) : (sinside (snot B (! C)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1608 (A B C D : S) : (sinside (snot B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1609 (A B C D : S) : (sinside (snot B (! C)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1610 (A B C D : S) : (sinside (snot B (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1611 (A B C D : S) : (sinside (snot B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1612 (A B C D : S) : (sinside (snot B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1613 (A B C D : S) : (sinside (snot B (! C)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1614 (A B C D : S) : (sinside (snot B (! C)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1615 (A B C D : S) : (soutside (snot B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1616 (A B C D : S) : (soutside (snot B (! C)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1617 (A B C D : S) : (soutside (snot B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1618 (A B C D : S) : (soutside (snot B (! C)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1619 (A B C D : S) : (soutside (snot B (! C)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1620 (A B C D : S) : (soutside (snot B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1621 (A B C D : S) : (soutside (snot B (! C)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1622 (A B C D : S) : (sinside (snot B (! D)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1623 (A B C D : S) : (sinside (snot B (! D)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1624 (A B C D : S) : (sinside (snot B (! D)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1625 (A B C D : S) : (sinside (snot B (! D)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1626 (A B C D : S) : (sinside (snot B (! D)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1627 (A B C D : S) : (sinside (snot B (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1628 (A B C D : S) : (sinside (snot B (! D)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1629 (A B C D : S) : (sinside (snot B (! D)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1630 (A B C D : S) : (sinside (snot B (! D)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1631 (A B C D : S) : (sinside (snot B (! D)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1632 (A B C D : S) : (sinside (snot B (! D)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1633 (A B C D : S) : (sinside (snot B (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1634 (A B C D : S) : (soutside (snot B (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1635 (A B C D : S) : (soutside (snot B (! D)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1636 (A B C D : S) : (soutside (snot B (! D)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1637 (A B C D : S) : (soutside (snot B (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1638 (A B C D : S) : (soutside (snot B (! D)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1639 (A B C D : S) : (soutside (snot B (! D)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1640 (A B C D : S) : (soutside (snot B (! D)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1641 (A B C D : S) : (sinside (sxor B (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1642 (A B C D : S) : (sinside (sxor B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1643 (A B C D : S) : (sinside (sxor B (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1644 (A B C D : S) : (soutside (sxor B (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1645 (A B C D : S) : (soutside (sxor B (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1646 (A B C D : S) : (soutside (sxor B (! C)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1647 (A B C D : S) : (sinside (sxor B (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1648 (A B C D : S) : (sinside (sxor B (! D)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1649 (A B C D : S) : (sinside (sxor B (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1650 (A B C D : S) : (soutside (sxor B (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1651 (A B C D : S) : (soutside (sxor B (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1652 (A B C D : S) : (soutside (sxor B (! D)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1653 (A B C D : S) : (sinside (sand C (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1654 (A B C D : S) : (sinside (sand C (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1655 (A B C D : S) : (sinside (sand C (! A)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1656 (A B C D : S) : (sinside (sand C (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1657 (A B C D : S) : (sinside (sand C (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1658 (A B C D : S) : (sinside (sand C (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1659 (A B C D : S) : (sinside (sand C (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1660 (A B C D : S) : (sinside (sand C (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1661 (A B C D : S) : (sinside (sand C (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1662 (A B C D : S) : (sinside (sand C (! A)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1663 (A B C D : S) : (sinside (sand C (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1664 (A B C D : S) : (sinside (sand C (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1665 (A B C D : S) : (sinside (sand C (! A)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1666 (A B C D : S) : (soutside (sand C (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1667 (A B C D : S) : (soutside (sand C (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1668 (A B C D : S) : (soutside (sand C (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1669 (A B C D : S) : (soutside (sand C (! A)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1670 (A B C D : S) : (soutside (sand C (! A)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1671 (A B C D : S) : (soutside (sand C (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1672 (A B C D : S) : (sinside (sand C (! B)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1673 (A B C D : S) : (sinside (sand C (! B)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1674 (A B C D : S) : (sinside (sand C (! B)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1675 (A B C D : S) : (sinside (sand C (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1676 (A B C D : S) : (sinside (sand C (! B)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1677 (A B C D : S) : (sinside (sand C (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1678 (A B C D : S) : (sinside (sand C (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1679 (A B C D : S) : (sinside (sand C (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1680 (A B C D : S) : (sinside (sand C (! B)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1681 (A B C D : S) : (sinside (sand C (! B)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1682 (A B C D : S) : (sinside (sand C (! B)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1683 (A B C D : S) : (sinside (sand C (! B)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1684 (A B C D : S) : (sinside (sand C (! B)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1685 (A B C D : S) : (soutside (sand C (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1686 (A B C D : S) : (soutside (sand C (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1687 (A B C D : S) : (soutside (sand C (! B)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1688 (A B C D : S) : (soutside (sand C (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1689 (A B C D : S) : (soutside (sand C (! B)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1690 (A B C D : S) : (soutside (sand C (! B)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1691 (A B C D : S) : (sinside (sand C (! D)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1692 (A B C D : S) : (sinside (sand C (! D)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1693 (A B C D : S) : (sinside (sand C (! D)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1694 (A B C D : S) : (sinside (sand C (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1695 (A B C D : S) : (sinside (sand C (! D)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1696 (A B C D : S) : (sinside (sand C (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1697 (A B C D : S) : (sinside (sand C (! D)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1698 (A B C D : S) : (sinside (sand C (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1699 (A B C D : S) : (sinside (sand C (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1700 (A B C D : S) : (sinside (sand C (! D)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1701 (A B C D : S) : (sinside (sand C (! D)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1702 (A B C D : S) : (sinside (sand C (! D)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1703 (A B C D : S) : (sinside (sand C (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1704 (A B C D : S) : (soutside (sand C (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1705 (A B C D : S) : (soutside (sand C (! D)) (sor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1706 (A B C D : S) : (soutside (sand C (! D)) (sor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1707 (A B C D : S) : (soutside (sand C (! D)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1708 (A B C D : S) : (soutside (sand C (! D)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1709 (A B C D : S) : (soutside (sand C (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1710 (A B C D : S) : (sinside (snot C (! D)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1711 (A B C D : S) : (sinside (snot C (! D)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1712 (A B C D : S) : (sinside (snot C (! D)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1713 (A B C D : S) : (sinside (snot C (! D)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1714 (A B C D : S) : (sinside (snot C (! D)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1715 (A B C D : S) : (sinside (snot C (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1716 (A B C D : S) : (sinside (snot C (! D)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1717 (A B C D : S) : (sinside (snot C (! D)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1718 (A B C D : S) : (sinside (snot C (! D)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1719 (A B C D : S) : (sinside (snot C (! D)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1720 (A B C D : S) : (sinside (snot C (! D)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1721 (A B C D : S) : (sinside (snot C (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1722 (A B C D : S) : (soutside (snot C (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1723 (A B C D : S) : (soutside (snot C (! D)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1724 (A B C D : S) : (soutside (snot C (! D)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1725 (A B C D : S) : (soutside (snot C (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1726 (A B C D : S) : (soutside (snot C (! D)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1727 (A B C D : S) : (soutside (snot C (! D)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1728 (A B C D : S) : (soutside (snot C (! D)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1729 (A B C D : S) : (sinside (sxor C (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1730 (A B C D : S) : (sinside (sxor C (! D)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1731 (A B C D : S) : (sinside (sxor C (! D)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1732 (A B C D : S) : (soutside (sxor C (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1733 (A B C D : S) : (soutside (sxor C (! D)) (sor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1734 (A B C D : S) : (soutside (sxor C (! D)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1735 (A B C D : S) : (sinside (sand D (! A)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1736 (A B C D : S) : (sinside (sand D (! A)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1737 (A B C D : S) : (sinside (sand D (! A)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1738 (A B C D : S) : (sinside (sand D (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1739 (A B C D : S) : (sinside (sand D (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1740 (A B C D : S) : (sinside (sand D (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1741 (A B C D : S) : (sinside (sand D (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1742 (A B C D : S) : (sinside (sand D (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1743 (A B C D : S) : (sinside (sand D (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1744 (A B C D : S) : (sinside (sand D (! A)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1745 (A B C D : S) : (sinside (sand D (! A)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1746 (A B C D : S) : (sinside (sand D (! A)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1747 (A B C D : S) : (sinside (sand D (! A)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1748 (A B C D : S) : (soutside (sand D (! A)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1749 (A B C D : S) : (soutside (sand D (! A)) (sor B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1750 (A B C D : S) : (soutside (sand D (! A)) (sor C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1751 (A B C D : S) : (soutside (sand D (! A)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1752 (A B C D : S) : (soutside (sand D (! A)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1753 (A B C D : S) : (soutside (sand D (! A)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1754 (A B C D : S) : (sinside (sand D (! B)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1755 (A B C D : S) : (sinside (sand D (! B)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1756 (A B C D : S) : (sinside (sand D (! B)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1757 (A B C D : S) : (sinside (sand D (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1758 (A B C D : S) : (sinside (sand D (! B)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1759 (A B C D : S) : (sinside (sand D (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1760 (A B C D : S) : (sinside (sand D (! B)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1761 (A B C D : S) : (sinside (sand D (! B)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1762 (A B C D : S) : (sinside (sand D (! B)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1763 (A B C D : S) : (sinside (sand D (! B)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1764 (A B C D : S) : (sinside (sand D (! B)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1765 (A B C D : S) : (sinside (sand D (! B)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1766 (A B C D : S) : (sinside (sand D (! B)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1767 (A B C D : S) : (soutside (sand D (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1768 (A B C D : S) : (soutside (sand D (! B)) (sor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1769 (A B C D : S) : (soutside (sand D (! B)) (sor C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1770 (A B C D : S) : (soutside (sand D (! B)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1771 (A B C D : S) : (soutside (sand D (! B)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1772 (A B C D : S) : (soutside (sand D (! B)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1773 (A B C D : S) : (sinside (sand D (! C)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1774 (A B C D : S) : (sinside (sand D (! C)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1775 (A B C D : S) : (sinside (sand D (! C)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1776 (A B C D : S) : (sinside (sand D (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1777 (A B C D : S) : (sinside (sand D (! C)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1778 (A B C D : S) : (sinside (sand D (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1779 (A B C D : S) : (sinside (sand D (! C)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1780 (A B C D : S) : (sinside (sand D (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1781 (A B C D : S) : (sinside (sand D (! C)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1782 (A B C D : S) : (sinside (sand D (! C)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1783 (A B C D : S) : (sinside (sand D (! C)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1784 (A B C D : S) : (sinside (sand D (! C)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1785 (A B C D : S) : (sinside (sand D (! C)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1786 (A B C D : S) : (soutside (sand D (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1787 (A B C D : S) : (soutside (sand D (! C)) (sor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1788 (A B C D : S) : (soutside (sand D (! C)) (sor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1789 (A B C D : S) : (soutside (sand D (! C)) (sor D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1790 (A B C D : S) : (soutside (sand D (! C)) (sor D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1791 (A B C D : S) : (soutside (sand D (! C)) (sor D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1792 (A B C D : S) : (soutside (snot (! A) B) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1793 (A B C D : S) : (soutside (snot (! A) B) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1794 (A B C D : S) : (soutside (snot (! A) C) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1795 (A B C D : S) : (soutside (snot (! A) C) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1796 (A B C D : S) : (soutside (snot (! A) D) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1797 (A B C D : S) : (soutside (snot (! A) D) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1798 (A B C D : S) : (snot (sor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1799 (A B C D : S) : (snot (sor (! A) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1800 (A B C D : S) : (snot (sor (! A) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1801 (A B C D : S) : (soutside (snot (! B) C) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1802 (A B C D : S) : (soutside (snot (! B) C) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1803 (A B C D : S) : (soutside (snot (! B) D) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1804 (A B C D : S) : (soutside (snot (! B) D) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1805 (A B C D : S) : (snot (sor (! B) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1806 (A B C D : S) : (snot (sor (! B) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1807 (A B C D : S) : (soutside (snot (! C) D) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1808 (A B C D : S) : (soutside (snot (! C) D) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1809 (A B C D : S) : (snot (sor (! C) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1810 (A B C D : S) : (soutside (sand A (! A)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1811 (A B C D : S) : (soutside (sand A (! A)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1812 (A B C D : S) : (soutside (sand A (! A)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1813 (A B C D : S) : (soutside (sand A (! A)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1814 (A B C D : S) : (soutside (sand A (! A)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1815 (A B C D : S) : (soutside (sand A (! A)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1816 (A B C D : S) : (soutside (sand A (! A)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1817 (A B C D : S) : (soutside (sand A (! A)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1818 (A B C D : S) : (soutside (sand A (! A)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1819 (A B C D : S) : (soutside (sand A (! A)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1820 (A B C D : S) : (soutside (sand A (! A)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1821 (A B C D : S) : (soutside (sand A (! A)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1822 (A B C D : S) : (soutside (sand A (! A)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1823 (A B C D : S) : (soutside (sand A (! A)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1824 (A B C D : S) : (soutside (sand A (! A)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1825 (A B C D : S) : (soutside (sand A (! A)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1826 (A B C D : S) : (soutside (sand A (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1827 (A B C D : S) : (soutside (sand A (! A)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1828 (A B C D : S) : (soutside (sand A (! A)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1829 (A B C D : S) : (soutside (sand A (! A)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1830 (A B C D : S) : (soutside (sand A (! A)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1831 (A B C D : S) : (soutside (sand A (! A)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1832 (A B C D : S) : (soutside (sand A (! A)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1833 (A B C D : S) : (soutside (sand A (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1834 (A B C D : S) : (soutside (sand A (! A)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1835 (A B C D : S) : (soutside (sand A (! A)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1836 (A B C D : S) : (soutside (sand A (! A)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1837 (A B C D : S) : (soutside (sand A (! A)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1838 (A B C D : S) : (soutside (sand A (! A)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1839 (A B C D : S) : (soutside (sand A (! A)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1840 (A B C D : S) : (soutside (sand A (! A)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1841 (A B C D : S) : (soutside (sand A (! B)) (sand A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1842 (A B C D : S) : (soutside (sand A (! C)) (sand A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1843 (A B C D : S) : (soutside (sand A (! D)) (sand A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1844 (A B C D : S) : (soutside (snot A (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1845 (A B C D : S) : (soutside (snot A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1846 (A B C D : S) : (soutside (snot A (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1847 (A B C D : S) : (soutside (snot A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1848 (A B C D : S) : (soutside (snot A (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1849 (A B C D : S) : (soutside (snot A (! D)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1850 (A B C D : S) : (soutside (sxor A (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1851 (A B C D : S) : (soutside (sxor A (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1852 (A B C D : S) : (soutside (sxor A (! D)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1853 (A B C D : S) : (soutside (sand B (! A)) (sand B (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1854 (A B C D : S) : (soutside (sand B (! C)) (sand B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1855 (A B C D : S) : (soutside (sand B (! D)) (sand B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1856 (A B C D : S) : (soutside (snot B (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1857 (A B C D : S) : (soutside (snot B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1858 (A B C D : S) : (soutside (snot B (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1859 (A B C D : S) : (soutside (snot B (! D)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1860 (A B C D : S) : (soutside (sxor B (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1861 (A B C D : S) : (soutside (sxor B (! D)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1862 (A B C D : S) : (soutside (sand C (! A)) (sand C (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1863 (A B C D : S) : (soutside (sand C (! B)) (sand C (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1864 (A B C D : S) : (soutside (sand C (! D)) (sand C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1865 (A B C D : S) : (soutside (snot C (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1866 (A B C D : S) : (soutside (snot C (! D)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1867 (A B C D : S) : (soutside (sxor C (! D)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1868 (A B C D : S) : (soutside (sand D (! A)) (sand D (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1869 (A B C D : S) : (soutside (sand D (! B)) (sand D (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1870 (A B C D : S) : (soutside (sand D (! C)) (sand D (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1871 (A B C D : S) : (snot (snot (! A) B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1872 (A B C D : S) : (snot (snot (! A) B) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1873 (A B C D : S) : (snot (snot (! A) B) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1874 (A B C D : S) : (snot (snot (! A) B) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1875 (A B C D : S) : (snot (snot (! A) B) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1876 (A B C D : S) : (snot (snot (! A) C) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1877 (A B C D : S) : (snot (snot (! A) C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1878 (A B C D : S) : (snot (snot (! A) C) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1879 (A B C D : S) : (snot (snot (! A) C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1880 (A B C D : S) : (snot (snot (! A) C) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1881 (A B C D : S) : (snot (snot (! A) D) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1882 (A B C D : S) : (snot (snot (! A) D) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1883 (A B C D : S) : (snot (snot (! A) D) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1884 (A B C D : S) : (snot (snot (! A) D) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1885 (A B C D : S) : (snot (snot (! A) D) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1886 (A B C D : S) : (sand (sor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1887 (A B C D : S) : (sand (sor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1888 (A B C D : S) : (sand (sor (! A) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1889 (A B C D : S) : (sand (sor (! A) (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1890 (A B C D : S) : (sand (sor (! A) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1891 (A B C D : S) : (sand (sor (! A) (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1892 (A B C D : S) : (snot (sxor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1893 (A B C D : S) : (snot (sxor (! A) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1894 (A B C D : S) : (snot (sxor (! A) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1895 (A B C D : S) : (snot (snot (! B) C) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1896 (A B C D : S) : (snot (snot (! B) C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1897 (A B C D : S) : (snot (snot (! B) C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1898 (A B C D : S) : (snot (snot (! B) C) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1899 (A B C D : S) : (snot (snot (! B) C) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1900 (A B C D : S) : (snot (snot (! B) D) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1901 (A B C D : S) : (snot (snot (! B) D) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1902 (A B C D : S) : (snot (snot (! B) D) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1903 (A B C D : S) : (snot (snot (! B) D) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1904 (A B C D : S) : (snot (snot (! B) D) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1905 (A B C D : S) : (sand (sor (! B) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1906 (A B C D : S) : (sand (sor (! B) (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1907 (A B C D : S) : (sand (sor (! B) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1908 (A B C D : S) : (sand (sor (! B) (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1909 (A B C D : S) : (snot (sxor (! B) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1910 (A B C D : S) : (snot (sxor (! B) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1911 (A B C D : S) : (snot (snot (! C) D) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1912 (A B C D : S) : (snot (snot (! C) D) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1913 (A B C D : S) : (snot (snot (! C) D) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1914 (A B C D : S) : (snot (snot (! C) D) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1915 (A B C D : S) : (snot (snot (! C) D) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1916 (A B C D : S) : (sand (sor (! C) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1917 (A B C D : S) : (sand (sor (! C) (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1918 (A B C D : S) : (snot (sxor (! C) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1919 (A B C D : S) : (sand (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1920 (A B C D : S) : (sand (sand A (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1921 (A B C D : S) : (sand (sand A (! A)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1922 (A B C D : S) : (sand (sand A (! A)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1923 (A B C D : S) : (sand (sand A (! A)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1924 (A B C D : S) : (sand (sand A (! A)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1925 (A B C D : S) : (snot (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1926 (A B C D : S) : (snot (sand A (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1927 (A B C D : S) : (snot (sand A (! A)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1928 (A B C D : S) : (snot (sand A (! A)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1929 (A B C D : S) : (snot (sand A (! A)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1930 (A B C D : S) : (snot (sand A (! A)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1931 (A B C D : S) : (snot (sand A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1932 (A B C D : S) : (snot (sand A (! B)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1933 (A B C D : S) : (snot (sand A (! B)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1934 (A B C D : S) : (snot (sand A (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1935 (A B C D : S) : (snot (sand A (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1936 (A B C D : S) : (snot (sand A (! C)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1937 (A B C D : S) : (snot (sand A (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1938 (A B C D : S) : (snot (sand A (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1939 (A B C D : S) : (snot (sand A (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1940 (A B C D : S) : (sand (snot A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1941 (A B C D : S) : (sand (snot A (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1942 (A B C D : S) : (sand (snot A (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1943 (A B C D : S) : (snot (sand B (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1944 (A B C D : S) : (snot (sand B (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1945 (A B C D : S) : (snot (sand B (! A)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1946 (A B C D : S) : (snot (sand B (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1947 (A B C D : S) : (snot (sand B (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1948 (A B C D : S) : (snot (sand B (! C)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1949 (A B C D : S) : (snot (sand B (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1950 (A B C D : S) : (snot (sand B (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1951 (A B C D : S) : (snot (sand B (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1952 (A B C D : S) : (sand (snot B (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1953 (A B C D : S) : (sand (snot B (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1954 (A B C D : S) : (snot (sand C (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1955 (A B C D : S) : (snot (sand C (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1956 (A B C D : S) : (snot (sand C (! A)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1957 (A B C D : S) : (snot (sand C (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1958 (A B C D : S) : (snot (sand C (! B)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1959 (A B C D : S) : (snot (sand C (! B)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1960 (A B C D : S) : (snot (sand C (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1961 (A B C D : S) : (snot (sand C (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1962 (A B C D : S) : (snot (sand C (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1963 (A B C D : S) : (sand (snot C (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1964 (A B C D : S) : (snot (sand D (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1965 (A B C D : S) : (snot (sand D (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1966 (A B C D : S) : (snot (sand D (! A)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1967 (A B C D : S) : (snot (sand D (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1968 (A B C D : S) : (snot (sand D (! B)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1969 (A B C D : S) : (snot (sand D (! B)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1970 (A B C D : S) : (snot (sand D (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1971 (A B C D : S) : (snot (sand D (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1972 (A B C D : S) : (snot (sand D (! C)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1973 (A B C D : S) : (sand (snot (! A) B) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1974 (A B C D : S) : (sand (snot (! A) C) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1975 (A B C D : S) : (sand (snot (! A) D) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1976 (A B C D : S) : (sand (sxor (! A) (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1977 (A B C D : S) : (sand (sxor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1978 (A B C D : S) : (sand (sxor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1979 (A B C D : S) : (sand (sxor (! A) (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1980 (A B C D : S) : (sand (sxor (! A) (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1981 (A B C D : S) : (sand (sxor (! A) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1982 (A B C D : S) : (sand (sxor (! A) (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1983 (A B C D : S) : (sand (sxor (! A) (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1984 (A B C D : S) : (sand (sxor (! A) (! D)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1985 (A B C D : S) : (sand (sxor (! A) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1986 (A B C D : S) : (sand (sxor (! A) (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1987 (A B C D : S) : (sand (sxor (! A) (! D)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1988 (A B C D : S) : (sand (snot (! B) C) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1989 (A B C D : S) : (sand (snot (! B) D) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1990 (A B C D : S) : (sand (sxor (! B) (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1991 (A B C D : S) : (sand (sxor (! B) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1992 (A B C D : S) : (sand (sxor (! B) (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1993 (A B C D : S) : (sand (sxor (! B) (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1994 (A B C D : S) : (sand (sxor (! B) (! D)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1995 (A B C D : S) : (sand (sxor (! B) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1996 (A B C D : S) : (sand (sxor (! B) (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1997 (A B C D : S) : (sand (sxor (! B) (! D)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1998 (A B C D : S) : (sand (snot (! C) D) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_1999 (A B C D : S) : (sand (sxor (! C) (! D)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2000 (A B C D : S) : (sand (sxor (! C) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2001 (A B C D : S) : (sand (sxor (! C) (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2002 (A B C D : S) : (sand (sxor (! C) (! D)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2003 (A B C D : S) : (sand (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2004 (A B C D : S) : (sand (sand A (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2005 (A B C D : S) : (sand (sand A (! A)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2006 (A B C D : S) : (sand (sand A (! A)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2007 (A B C D : S) : (sand (sand A (! A)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2008 (A B C D : S) : (sand (sand A (! A)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2009 (A B C D : S) : (snot (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2010 (A B C D : S) : (snot (sand A (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2011 (A B C D : S) : (snot (sand A (! A)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2012 (A B C D : S) : (snot (sand A (! A)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2013 (A B C D : S) : (snot (sand A (! A)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2014 (A B C D : S) : (snot (sand A (! A)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2015 (A B C D : S) : (snot (sand A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2016 (A B C D : S) : (snot (sand A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2017 (A B C D : S) : (snot (sand A (! D)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2018 (A B C D : S) : (sand (snot A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2019 (A B C D : S) : (sand (snot A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2020 (A B C D : S) : (sand (snot A (! D)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2021 (A B C D : S) : (sand (sxor A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2022 (A B C D : S) : (sand (sxor A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2023 (A B C D : S) : (sand (sxor A (! D)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2024 (A B C D : S) : (snot (sand B (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2025 (A B C D : S) : (snot (sand B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2026 (A B C D : S) : (snot (sand B (! D)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2027 (A B C D : S) : (sand (snot B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2028 (A B C D : S) : (sand (snot B (! D)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2029 (A B C D : S) : (sand (sxor B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2030 (A B C D : S) : (sand (sxor B (! D)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2031 (A B C D : S) : (snot (sand C (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2032 (A B C D : S) : (snot (sand C (! B)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2033 (A B C D : S) : (snot (sand C (! D)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2034 (A B C D : S) : (sand (snot C (! D)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2035 (A B C D : S) : (sand (sxor C (! D)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2036 (A B C D : S) : (snot (sand D (! A)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2037 (A B C D : S) : (snot (sand D (! B)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2038 (A B C D : S) : (snot (sand D (! C)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2039 (A B C D : S) : (sinside (sor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2040 (A B C D : S) : (sinside (sor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2041 (A B C D : S) : (soutside (sor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2042 (A B C D : S) : (sinside (sor (! A) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2043 (A B C D : S) : (sinside (sor (! A) (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2044 (A B C D : S) : (soutside (sor (! A) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2045 (A B C D : S) : (sinside (sor (! A) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2046 (A B C D : S) : (sinside (sor (! A) (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2047 (A B C D : S) : (soutside (sor (! A) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2048 (A B C D : S) : (sinside (sor (! B) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2049 (A B C D : S) : (sinside (sor (! B) (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2050 (A B C D : S) : (soutside (sor (! B) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2051 (A B C D : S) : (sinside (sor (! B) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2052 (A B C D : S) : (sinside (sor (! B) (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2053 (A B C D : S) : (soutside (sor (! B) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2054 (A B C D : S) : (sinside (sor (! C) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2055 (A B C D : S) : (sinside (sor (! C) (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2056 (A B C D : S) : (soutside (sor (! C) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2057 (A B C D : S) : (sinside (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2058 (A B C D : S) : (sinside (sand A (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2059 (A B C D : S) : (sinside (sand A (! A)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2060 (A B C D : S) : (sinside (sand A (! A)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2061 (A B C D : S) : (sinside (sand A (! A)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2062 (A B C D : S) : (sinside (sand A (! A)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2063 (A B C D : S) : (sinside (snot A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2064 (A B C D : S) : (sinside (snot A (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2065 (A B C D : S) : (sinside (snot A (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2066 (A B C D : S) : (sinside (snot B (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2067 (A B C D : S) : (sinside (snot B (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2068 (A B C D : S) : (sinside (snot C (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2069 (A B C D : S) : (sinside (snot (! A) B) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2070 (A B C D : S) : (soutside (snot (! A) B) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2071 (A B C D : S) : (soutside (snot (! A) B) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2072 (A B C D : S) : (soutside (snot (! A) B) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2073 (A B C D : S) : (soutside (snot (! A) B) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2074 (A B C D : S) : (soutside (snot (! A) B) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2075 (A B C D : S) : (sinside (snot (! A) C) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2076 (A B C D : S) : (soutside (snot (! A) C) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2077 (A B C D : S) : (soutside (snot (! A) C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2078 (A B C D : S) : (soutside (snot (! A) C) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2079 (A B C D : S) : (soutside (snot (! A) C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2080 (A B C D : S) : (soutside (snot (! A) C) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2081 (A B C D : S) : (sinside (snot (! A) D) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2082 (A B C D : S) : (soutside (snot (! A) D) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2083 (A B C D : S) : (soutside (snot (! A) D) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2084 (A B C D : S) : (soutside (snot (! A) D) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2085 (A B C D : S) : (soutside (snot (! A) D) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2086 (A B C D : S) : (soutside (snot (! A) D) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2087 (A B C D : S) : (sinside (sxor (! A) (! B)) (snot (! A) B)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2088 (A B C D : S) : (sinside (sxor (! A) (! B)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2089 (A B C D : S) : (sinside (sxor (! A) (! B)) (snot A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2090 (A B C D : S) : (sinside (sxor (! A) (! B)) (sxor A (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2091 (A B C D : S) : (soutside (sxor (! A) (! B)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2092 (A B C D : S) : (sinside (sxor (! A) (! C)) (snot (! A) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2093 (A B C D : S) : (sinside (sxor (! A) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2094 (A B C D : S) : (sinside (sxor (! A) (! C)) (snot A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2095 (A B C D : S) : (sinside (sxor (! A) (! C)) (sxor A (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2096 (A B C D : S) : (soutside (sxor (! A) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2097 (A B C D : S) : (sinside (sxor (! A) (! D)) (snot (! A) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2098 (A B C D : S) : (sinside (sxor (! A) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2099 (A B C D : S) : (sinside (sxor (! A) (! D)) (snot A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2100 (A B C D : S) : (sinside (sxor (! A) (! D)) (sxor A (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2101 (A B C D : S) : (soutside (sxor (! A) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2102 (A B C D : S) : (sinside (snot (! B) C) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2103 (A B C D : S) : (soutside (snot (! B) C) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2104 (A B C D : S) : (soutside (snot (! B) C) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2105 (A B C D : S) : (soutside (snot (! B) C) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2106 (A B C D : S) : (soutside (snot (! B) C) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2107 (A B C D : S) : (soutside (snot (! B) C) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2108 (A B C D : S) : (sinside (snot (! B) D) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2109 (A B C D : S) : (soutside (snot (! B) D) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2110 (A B C D : S) : (soutside (snot (! B) D) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2111 (A B C D : S) : (soutside (snot (! B) D) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2112 (A B C D : S) : (soutside (snot (! B) D) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2113 (A B C D : S) : (soutside (snot (! B) D) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2114 (A B C D : S) : (sinside (sxor (! B) (! C)) (snot (! B) C)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2115 (A B C D : S) : (sinside (sxor (! B) (! C)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2116 (A B C D : S) : (sinside (sxor (! B) (! C)) (snot B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2117 (A B C D : S) : (sinside (sxor (! B) (! C)) (sxor B (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2118 (A B C D : S) : (soutside (sxor (! B) (! C)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2119 (A B C D : S) : (sinside (sxor (! B) (! D)) (snot (! B) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2120 (A B C D : S) : (sinside (sxor (! B) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2121 (A B C D : S) : (sinside (sxor (! B) (! D)) (snot B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2122 (A B C D : S) : (sinside (sxor (! B) (! D)) (sxor B (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2123 (A B C D : S) : (soutside (sxor (! B) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2124 (A B C D : S) : (sinside (snot (! C) D) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2125 (A B C D : S) : (soutside (snot (! C) D) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2126 (A B C D : S) : (soutside (snot (! C) D) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2127 (A B C D : S) : (soutside (snot (! C) D) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2128 (A B C D : S) : (soutside (snot (! C) D) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2129 (A B C D : S) : (soutside (snot (! C) D) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2130 (A B C D : S) : (sinside (sxor (! C) (! D)) (snot (! C) D)) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2131 (A B C D : S) : (sinside (sxor (! C) (! D)) (sand A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2132 (A B C D : S) : (sinside (sxor (! C) (! D)) (snot C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2133 (A B C D : S) : (sinside (sxor (! C) (! D)) (sxor C (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2134 (A B C D : S) : (soutside (sxor (! C) (! D)) (sor A (! A))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2135 (A B C D : S) : (sinside (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2136 (A B C D : S) : (sinside (sand A (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2137 (A B C D : S) : (sinside (sand A (! A)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2138 (A B C D : S) : (sinside (sand A (! A)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2139 (A B C D : S) : (sinside (sand A (! A)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2140 (A B C D : S) : (sinside (sand A (! A)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2141 (A B C D : S) : (soutside (sand A (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2142 (A B C D : S) : (soutside (sand A (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2143 (A B C D : S) : (soutside (sand A (! A)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2144 (A B C D : S) : (soutside (sand A (! A)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2145 (A B C D : S) : (soutside (sand A (! A)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2146 (A B C D : S) : (soutside (sand A (! A)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2147 (A B C D : S) : (soutside (sand A (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2148 (A B C D : S) : (soutside (sand A (! B)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2149 (A B C D : S) : (soutside (sand A (! B)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2150 (A B C D : S) : (soutside (sand A (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2151 (A B C D : S) : (soutside (sand A (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2152 (A B C D : S) : (soutside (sand A (! C)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2153 (A B C D : S) : (soutside (sand A (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2154 (A B C D : S) : (soutside (sand A (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2155 (A B C D : S) : (soutside (sand A (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2156 (A B C D : S) : (sinside (snot A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2157 (A B C D : S) : (sinside (snot A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2158 (A B C D : S) : (sinside (snot A (! D)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2159 (A B C D : S) : (sinside (sxor A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2160 (A B C D : S) : (sinside (sxor A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2161 (A B C D : S) : (sinside (sxor A (! D)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2162 (A B C D : S) : (soutside (sand B (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2163 (A B C D : S) : (soutside (sand B (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2164 (A B C D : S) : (soutside (sand B (! A)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2165 (A B C D : S) : (soutside (sand B (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2166 (A B C D : S) : (soutside (sand B (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2167 (A B C D : S) : (soutside (sand B (! C)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2168 (A B C D : S) : (soutside (sand B (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2169 (A B C D : S) : (soutside (sand B (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2170 (A B C D : S) : (soutside (sand B (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2171 (A B C D : S) : (sinside (snot B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2172 (A B C D : S) : (sinside (snot B (! D)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2173 (A B C D : S) : (sinside (sxor B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2174 (A B C D : S) : (sinside (sxor B (! D)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2175 (A B C D : S) : (soutside (sand C (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2176 (A B C D : S) : (soutside (sand C (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2177 (A B C D : S) : (soutside (sand C (! A)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2178 (A B C D : S) : (soutside (sand C (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2179 (A B C D : S) : (soutside (sand C (! B)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2180 (A B C D : S) : (soutside (sand C (! B)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2181 (A B C D : S) : (soutside (sand C (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2182 (A B C D : S) : (soutside (sand C (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2183 (A B C D : S) : (soutside (sand C (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2184 (A B C D : S) : (sinside (snot C (! D)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2185 (A B C D : S) : (sinside (sxor C (! D)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2186 (A B C D : S) : (soutside (sand D (! A)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2187 (A B C D : S) : (soutside (sand D (! A)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2188 (A B C D : S) : (soutside (sand D (! A)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2189 (A B C D : S) : (soutside (sand D (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2190 (A B C D : S) : (soutside (sand D (! B)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2191 (A B C D : S) : (soutside (sand D (! B)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2192 (A B C D : S) : (soutside (sand D (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2193 (A B C D : S) : (soutside (sand D (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2194 (A B C D : S) : (soutside (sand D (! C)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2195 (A B C D : S) : (snot (sor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2196 (A B C D : S) : (sxor (sor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2197 (A B C D : S) : (snot (sor (! A) (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2198 (A B C D : S) : (sxor (sor (! A) (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2199 (A B C D : S) : (snot (sor (! A) (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2200 (A B C D : S) : (sxor (sor (! A) (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2201 (A B C D : S) : (snot (sor (! B) (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2202 (A B C D : S) : (sxor (sor (! B) (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2203 (A B C D : S) : (snot (sor (! B) (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2204 (A B C D : S) : (sxor (sor (! B) (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2205 (A B C D : S) : (snot (sor (! C) (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2206 (A B C D : S) : (sxor (sor (! C) (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2207 (A B C D : S) : (soutside (sand A (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2208 (A B C D : S) : (soutside (sand A (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2209 (A B C D : S) : (soutside (sand A (! A)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2210 (A B C D : S) : (soutside (sand A (! A)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2211 (A B C D : S) : (soutside (sand A (! A)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2212 (A B C D : S) : (soutside (sand A (! A)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2213 (A B C D : S) : (soutside (sand A (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2214 (A B C D : S) : (soutside (sand A (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2215 (A B C D : S) : (soutside (sand A (! D)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2216 (A B C D : S) : (soutside (sand B (! A)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2217 (A B C D : S) : (soutside (sand B (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2218 (A B C D : S) : (soutside (sand B (! D)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2219 (A B C D : S) : (soutside (sand C (! A)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2220 (A B C D : S) : (soutside (sand C (! B)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2221 (A B C D : S) : (soutside (sand C (! D)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2222 (A B C D : S) : (soutside (sand D (! A)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2223 (A B C D : S) : (soutside (sand D (! B)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2224 (A B C D : S) : (soutside (sand D (! C)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2225 (A B C D : S) : (snot (sxor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2226 (A B C D : S) : (snot (sxor (! A) (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2227 (A B C D : S) : (snot (sxor (! A) (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2228 (A B C D : S) : (snot (sxor (! B) (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2229 (A B C D : S) : (snot (sxor (! B) (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2230 (A B C D : S) : (snot (sxor (! C) (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2231 (A B C D : S) : (snot (sxor (! A) (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2232 (A B C D : S) : (sxor (sxor (! A) (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2233 (A B C D : S) : (snot (sxor (! A) (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2234 (A B C D : S) : (sxor (sxor (! A) (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2235 (A B C D : S) : (snot (sxor (! A) (! D)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2236 (A B C D : S) : (sxor (sxor (! A) (! D)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2237 (A B C D : S) : (snot (sxor (! B) (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2238 (A B C D : S) : (sxor (sxor (! B) (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2239 (A B C D : S) : (snot (sxor (! B) (! D)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2240 (A B C D : S) : (sxor (sxor (! B) (! D)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2241 (A B C D : S) : (snot (sxor (! C) (! D)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2242 (A B C D : S) : (sxor (sxor (! C) (! D)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2243 (A B C D : S) : (soutside (sor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2244 (A B C D : S) : (soutside (sor (! A) (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2245 (A B C D : S) : (soutside (sor (! A) (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2246 (A B C D : S) : (soutside (sor (! B) (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2247 (A B C D : S) : (soutside (sor (! B) (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2248 (A B C D : S) : (soutside (sor (! C) (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2249 (A B C D : S) : (soutside (sxor (! A) (! B)) (sor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2250 (A B C D : S) : (soutside (sxor (! A) (! C)) (sor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2251 (A B C D : S) : (soutside (sxor (! A) (! D)) (sor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2252 (A B C D : S) : (soutside (sxor (! B) (! C)) (sor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2253 (A B C D : S) : (soutside (sxor (! B) (! D)) (sor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2254 (A B C D : S) : (soutside (sxor (! C) (! D)) (sor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2255 (A B C D : S) : (soutside (sxor (! A) (! B)) (sxor (! A) (! B))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2256 (A B C D : S) : (soutside (sxor (! A) (! C)) (sxor (! A) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2257 (A B C D : S) : (soutside (sxor (! A) (! D)) (sxor (! A) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2258 (A B C D : S) : (soutside (sxor (! B) (! C)) (sxor (! B) (! C))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2259 (A B C D : S) : (soutside (sxor (! B) (! D)) (sxor (! B) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_0_2260 (A B C D : S) : (soutside (sxor (! C) (! D)) (sxor (! C) (! D))) = false := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_0 (A B C D : S) : (snot A (! A)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_1 (A B C D : S) : (soutside A (! A)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_2 (A B C D : S) : (sor (sand A (! A)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_3 (A B C D : S) : (sor (sand A (! B)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_4 (A B C D : S) : (sor (sand A (! C)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_5 (A B C D : S) : (sor (sand A (! D)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_6 (A B C D : S) : (sor (snot A (! B)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_7 (A B C D : S) : (sor (snot A (! C)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_8 (A B C D : S) : (sor (snot A (! D)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_9 (A B C D : S) : (sand (sor A (! A)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_10 (A B C D : S) : (sand (sor A (! B)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_11 (A B C D : S) : (sand (sor A (! C)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_12 (A B C D : S) : (sand (sor A (! D)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_13 (A B C D : S) : (sand A (sor A (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_14 (A B C D : S) : (sand A (sor A (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_15 (A B C D : S) : (sand A (sor A (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_16 (A B C D : S) : (sand A (sor A (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_17 (A B C D : S) : (sor A (sand A (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_18 (A B C D : S) : (sor A (sand A (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_19 (A B C D : S) : (sor A (sand A (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_20 (A B C D : S) : (sor A (sand A (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_21 (A B C D : S) : (sor A (snot A (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_22 (A B C D : S) : (sor A (snot A (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_23 (A B C D : S) : (sor A (snot A (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_24 (A B C D : S) : (sxor (sand A (! A)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_25 (A B C D : S) : (snot A (snot (! A) B)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_26 (A B C D : S) : (snot A (snot (! A) C)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_27 (A B C D : S) : (snot A (snot (! A) D)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_28 (A B C D : S) : (snot A (sand A (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_29 (A B C D : S) : (snot A (sand B (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_30 (A B C D : S) : (snot A (sand C (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_31 (A B C D : S) : (snot A (sand D (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_32 (A B C D : S) : (sxor A (sand A (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_33 (A B C D : S) : (sinside (sor A (! A)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_34 (A B C D : S) : (sinside (sor A (! B)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_35 (A B C D : S) : (sinside (sor A (! C)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_36 (A B C D : S) : (sinside (sor A (! D)) A) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_37 (A B C D : S) : (sinside A (sor A (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_38 (A B C D : S) : (sinside A (sor A (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_39 (A B C D : S) : (sinside A (sor A (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_40 (A B C D : S) : (sinside A (sor A (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_41 (A B C D : S) : (soutside A (snot (! A) B)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_42 (A B C D : S) : (soutside A (snot (! A) C)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_43 (A B C D : S) : (soutside A (snot (! A) D)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_44 (A B C D : S) : (soutside A (sand A (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_45 (A B C D : S) : (soutside A (sand B (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_46 (A B C D : S) : (soutside A (sand C (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_47 (A B C D : S) : (soutside A (sand D (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_48 (A B C D : S) : (snot (sor A (! A)) (! A)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_49 (A B C D : S) : (sxor (sor A (! A)) (! A)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_50 (A B C D : S) : (snot (sor A (! B)) (! A)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_51 (A B C D : S) : (snot (sor A (! C)) (! A)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_52 (A B C D : S) : (snot (sor A (! D)) (! A)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_53 (A B C D : S) : (sxor (! A) (sor A (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_54 (A B C D : S) : (sxor (sxor (! A) (! B)) B) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_55 (A B C D : S) : (sxor (sxor (! A) (! C)) C) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_56 (A B C D : S) : (sxor (sxor (! A) (! D)) D) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_57 (A B C D : S) : (sxor (sxor A (! B)) (! B)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_58 (A B C D : S) : (sxor (sxor A (! C)) (! C)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_59 (A B C D : S) : (sxor (sxor A (! D)) (! D)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_60 (A B C D : S) : (sxor (! B) (sxor A (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_61 (A B C D : S) : (sxor (! C) (sxor A (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_62 (A B C D : S) : (sxor (! D) (sxor A (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_63 (A B C D : S) : (sxor B (sxor (! A) (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_64 (A B C D : S) : (sxor C (sxor (! A) (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_65 (A B C D : S) : (sxor D (sxor (! A) (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_66 (A B C D : S) : (soutside (sor A (! A)) (! A)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_67 (A B C D : S) : (soutside (sor A (! B)) (! A)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_68 (A B C D : S) : (soutside (sor A (! C)) (! A)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_69 (A B C D : S) : (soutside (sor A (! D)) (! A)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_70 (A B C D : S) : (sxor (snot (! A) B) (sor A (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_71 (A B C D : S) : (sxor (snot (! A) C) (sor A (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_72 (A B C D : S) : (sxor (snot (! A) D) (sor A (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_73 (A B C D : S) : (sor (sand A (! B)) (snot A (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_74 (A B C D : S) : (sor (sand A (! C)) (snot A (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_75 (A B C D : S) : (sor (sand A (! D)) (snot A (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_76 (A B C D : S) : (sor (snot A (! B)) (sand A (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_77 (A B C D : S) : (sor (snot A (! C)) (sand A (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_78 (A B C D : S) : (sor (snot A (! D)) (sand A (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_79 (A B C D : S) : (snot (sor A (! B)) (snot (! A) B)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_80 (A B C D : S) : (sxor (sor A (! B)) (snot (! A) B)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_81 (A B C D : S) : (snot (sor A (! C)) (snot (! A) C)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_82 (A B C D : S) : (sxor (sor A (! C)) (snot (! A) C)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_83 (A B C D : S) : (snot (sor A (! D)) (snot (! A) D)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_84 (A B C D : S) : (sxor (sor A (! D)) (snot (! A) D)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_85 (A B C D : S) : (sxor (sand A (! B)) (snot A (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_86 (A B C D : S) : (sxor (sand A (! C)) (snot A (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_87 (A B C D : S) : (sxor (sand A (! D)) (snot A (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_88 (A B C D : S) : (sxor (snot A (! B)) (sand A (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_89 (A B C D : S) : (sxor (snot A (! C)) (sand A (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_90 (A B C D : S) : (sxor (snot A (! D)) (sand A (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_91 (A B C D : S) : (soutside (sor A (! B)) (snot (! A) B)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_92 (A B C D : S) : (soutside (sor A (! C)) (snot (! A) C)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_93 (A B C D : S) : (soutside (sor A (! D)) (snot (! A) D)) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_94 (A B C D : S) : (sxor (sor (! A) (! B)) (sor B (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_95 (A B C D : S) : (sxor (sor (! A) (! C)) (sor C (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_96 (A B C D : S) : (sxor (sor (! A) (! D)) (sor D (! A))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_97 (A B C D : S) : (sxor (sor B (! A)) (sor (! A) (! B))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_98 (A B C D : S) : (sxor (sor C (! A)) (sor (! A) (! C))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_A_99 (A B C D : S) : (sxor (sor D (! A)) (sor (! A) (! D))) = A := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_0 (A B C D : S) : (snot B (! B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_1 (A B C D : S) : (soutside B (! B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_2 (A B C D : S) : (sor (sand A (! A)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_3 (A B C D : S) : (sor (snot A (! B)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_4 (A B C D : S) : (sand (sor A (! A)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_5 (A B C D : S) : (sor (sand B (! A)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_6 (A B C D : S) : (sor (sand B (! C)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_7 (A B C D : S) : (sor (sand B (! D)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_8 (A B C D : S) : (sor (snot B (! C)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_9 (A B C D : S) : (sor (snot B (! D)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_10 (A B C D : S) : (sand (sor B (! A)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_11 (A B C D : S) : (sand (sor B (! C)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_12 (A B C D : S) : (sand (sor B (! D)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_13 (A B C D : S) : (sand B (sor A (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_14 (A B C D : S) : (sand B (sor B (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_15 (A B C D : S) : (sand B (sor B (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_16 (A B C D : S) : (sand B (sor B (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_17 (A B C D : S) : (sor B (sand A (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_18 (A B C D : S) : (sor B (snot A (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_19 (A B C D : S) : (sor B (sand B (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_20 (A B C D : S) : (sor B (sand B (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_21 (A B C D : S) : (sor B (sand B (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_22 (A B C D : S) : (sor B (snot B (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_23 (A B C D : S) : (sor B (snot B (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_24 (A B C D : S) : (sxor (sand A (! A)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_25 (A B C D : S) : (snot B (snot (! A) B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_26 (A B C D : S) : (snot B (snot (! B) C)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_27 (A B C D : S) : (snot B (snot (! B) D)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_28 (A B C D : S) : (snot B (sand A (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_29 (A B C D : S) : (snot B (sand A (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_30 (A B C D : S) : (snot B (sand C (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_31 (A B C D : S) : (snot B (sand D (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_32 (A B C D : S) : (sxor B (sand A (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_33 (A B C D : S) : (sinside (sor A (! A)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_34 (A B C D : S) : (sinside (sor B (! A)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_35 (A B C D : S) : (sinside (sor B (! C)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_36 (A B C D : S) : (sinside (sor B (! D)) B) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_37 (A B C D : S) : (sinside B (sor A (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_38 (A B C D : S) : (sinside B (sor B (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_39 (A B C D : S) : (sinside B (sor B (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_40 (A B C D : S) : (sinside B (sor B (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_41 (A B C D : S) : (soutside B (snot (! A) B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_42 (A B C D : S) : (soutside B (snot (! B) C)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_43 (A B C D : S) : (soutside B (snot (! B) D)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_44 (A B C D : S) : (soutside B (sand A (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_45 (A B C D : S) : (soutside B (sand A (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_46 (A B C D : S) : (soutside B (sand C (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_47 (A B C D : S) : (soutside B (sand D (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_48 (A B C D : S) : (snot (sor A (! A)) (! B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_49 (A B C D : S) : (sxor (sor A (! A)) (! B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_50 (A B C D : S) : (snot (sor B (! A)) (! B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_51 (A B C D : S) : (snot (sor B (! C)) (! B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_52 (A B C D : S) : (snot (sor B (! D)) (! B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_53 (A B C D : S) : (sxor (! B) (sor A (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_54 (A B C D : S) : (sxor (sxor (! A) (! B)) A) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_55 (A B C D : S) : (sxor (sxor (! B) (! C)) C) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_56 (A B C D : S) : (sxor (sxor (! B) (! D)) D) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_57 (A B C D : S) : (sxor (sxor A (! B)) (! A)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_58 (A B C D : S) : (sxor (sxor B (! C)) (! C)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_59 (A B C D : S) : (sxor (sxor B (! D)) (! D)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_60 (A B C D : S) : (sxor (! A) (sxor A (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_61 (A B C D : S) : (sxor (! C) (sxor B (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_62 (A B C D : S) : (sxor (! D) (sxor B (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_63 (A B C D : S) : (sxor A (sxor (! A) (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_64 (A B C D : S) : (sxor C (sxor (! B) (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_65 (A B C D : S) : (sxor D (sxor (! B) (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_66 (A B C D : S) : (soutside (sor A (! A)) (! B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_67 (A B C D : S) : (soutside (sor B (! A)) (! B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_68 (A B C D : S) : (soutside (sor B (! C)) (! B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_69 (A B C D : S) : (soutside (sor B (! D)) (! B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_70 (A B C D : S) : (sxor (snot (! A) B) (sor B (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_71 (A B C D : S) : (sxor (snot (! B) C) (sor B (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_72 (A B C D : S) : (sxor (snot (! B) D) (sor B (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_73 (A B C D : S) : (sor (snot A (! B)) (sand B (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_74 (A B C D : S) : (sor (sand B (! A)) (snot A (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_75 (A B C D : S) : (sor (sand B (! C)) (snot B (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_76 (A B C D : S) : (sor (sand B (! D)) (snot B (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_77 (A B C D : S) : (sor (snot B (! C)) (sand B (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_78 (A B C D : S) : (sor (snot B (! D)) (sand B (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_79 (A B C D : S) : (snot (sor B (! A)) (snot (! A) B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_80 (A B C D : S) : (sxor (sor B (! A)) (snot (! A) B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_81 (A B C D : S) : (snot (sor B (! C)) (snot (! B) C)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_82 (A B C D : S) : (sxor (sor B (! C)) (snot (! B) C)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_83 (A B C D : S) : (snot (sor B (! D)) (snot (! B) D)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_84 (A B C D : S) : (sxor (sor B (! D)) (snot (! B) D)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_85 (A B C D : S) : (sxor (snot A (! B)) (sand B (! A))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_86 (A B C D : S) : (sxor (sand B (! A)) (snot A (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_87 (A B C D : S) : (sxor (sand B (! C)) (snot B (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_88 (A B C D : S) : (sxor (sand B (! D)) (snot B (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_89 (A B C D : S) : (sxor (snot B (! C)) (sand B (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_90 (A B C D : S) : (sxor (snot B (! D)) (sand B (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_91 (A B C D : S) : (soutside (sor B (! A)) (snot (! A) B)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_92 (A B C D : S) : (soutside (sor B (! C)) (snot (! B) C)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_93 (A B C D : S) : (soutside (sor B (! D)) (snot (! B) D)) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_94 (A B C D : S) : (sxor (sor (! A) (! B)) (sor A (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_95 (A B C D : S) : (sxor (sor (! B) (! C)) (sor C (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_96 (A B C D : S) : (sxor (sor (! B) (! D)) (sor D (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_97 (A B C D : S) : (sxor (sor A (! B)) (sor (! A) (! B))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_98 (A B C D : S) : (sxor (sor C (! B)) (sor (! B) (! C))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_B_99 (A B C D : S) : (sxor (sor D (! B)) (sor (! B) (! D))) = B := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_0 (A B C D : S) : (snot C (! C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_1 (A B C D : S) : (soutside C (! C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_2 (A B C D : S) : (sor (sand A (! A)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_3 (A B C D : S) : (sor (snot A (! C)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_4 (A B C D : S) : (sand (sor A (! A)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_5 (A B C D : S) : (sor (snot B (! C)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_6 (A B C D : S) : (sor (sand C (! A)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_7 (A B C D : S) : (sor (sand C (! B)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_8 (A B C D : S) : (sor (sand C (! D)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_9 (A B C D : S) : (sor (snot C (! D)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_10 (A B C D : S) : (sand (sor C (! A)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_11 (A B C D : S) : (sand (sor C (! B)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_12 (A B C D : S) : (sand (sor C (! D)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_13 (A B C D : S) : (sand C (sor A (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_14 (A B C D : S) : (sand C (sor C (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_15 (A B C D : S) : (sand C (sor C (! B))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_16 (A B C D : S) : (sand C (sor C (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_17 (A B C D : S) : (sor C (sand A (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_18 (A B C D : S) : (sor C (snot A (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_19 (A B C D : S) : (sor C (snot B (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_20 (A B C D : S) : (sor C (sand C (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_21 (A B C D : S) : (sor C (sand C (! B))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_22 (A B C D : S) : (sor C (sand C (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_23 (A B C D : S) : (sor C (snot C (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_24 (A B C D : S) : (sxor (sand A (! A)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_25 (A B C D : S) : (snot C (snot (! A) C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_26 (A B C D : S) : (snot C (snot (! B) C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_27 (A B C D : S) : (snot C (snot (! C) D)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_28 (A B C D : S) : (snot C (sand A (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_29 (A B C D : S) : (snot C (sand A (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_30 (A B C D : S) : (snot C (sand B (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_31 (A B C D : S) : (snot C (sand D (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_32 (A B C D : S) : (sxor C (sand A (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_33 (A B C D : S) : (sinside (sor A (! A)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_34 (A B C D : S) : (sinside (sor C (! A)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_35 (A B C D : S) : (sinside (sor C (! B)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_36 (A B C D : S) : (sinside (sor C (! D)) C) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_37 (A B C D : S) : (sinside C (sor A (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_38 (A B C D : S) : (sinside C (sor C (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_39 (A B C D : S) : (sinside C (sor C (! B))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_40 (A B C D : S) : (sinside C (sor C (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_41 (A B C D : S) : (soutside C (snot (! A) C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_42 (A B C D : S) : (soutside C (snot (! B) C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_43 (A B C D : S) : (soutside C (snot (! C) D)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_44 (A B C D : S) : (soutside C (sand A (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_45 (A B C D : S) : (soutside C (sand A (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_46 (A B C D : S) : (soutside C (sand B (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_47 (A B C D : S) : (soutside C (sand D (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_48 (A B C D : S) : (snot (sor A (! A)) (! C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_49 (A B C D : S) : (sxor (sor A (! A)) (! C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_50 (A B C D : S) : (snot (sor C (! A)) (! C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_51 (A B C D : S) : (snot (sor C (! B)) (! C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_52 (A B C D : S) : (snot (sor C (! D)) (! C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_53 (A B C D : S) : (sxor (! C) (sor A (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_54 (A B C D : S) : (sxor (sxor (! A) (! C)) A) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_55 (A B C D : S) : (sxor (sxor (! B) (! C)) B) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_56 (A B C D : S) : (sxor (sxor (! C) (! D)) D) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_57 (A B C D : S) : (sxor (sxor A (! C)) (! A)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_58 (A B C D : S) : (sxor (sxor B (! C)) (! B)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_59 (A B C D : S) : (sxor (sxor C (! D)) (! D)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_60 (A B C D : S) : (sxor (! A) (sxor A (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_61 (A B C D : S) : (sxor (! B) (sxor B (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_62 (A B C D : S) : (sxor (! D) (sxor C (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_63 (A B C D : S) : (sxor A (sxor (! A) (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_64 (A B C D : S) : (sxor B (sxor (! B) (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_65 (A B C D : S) : (sxor D (sxor (! C) (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_66 (A B C D : S) : (soutside (sor A (! A)) (! C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_67 (A B C D : S) : (soutside (sor C (! A)) (! C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_68 (A B C D : S) : (soutside (sor C (! B)) (! C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_69 (A B C D : S) : (soutside (sor C (! D)) (! C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_70 (A B C D : S) : (sxor (snot (! A) C) (sor C (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_71 (A B C D : S) : (sxor (snot (! B) C) (sor C (! B))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_72 (A B C D : S) : (sxor (snot (! C) D) (sor C (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_73 (A B C D : S) : (sor (snot A (! C)) (sand C (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_74 (A B C D : S) : (sor (snot B (! C)) (sand C (! B))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_75 (A B C D : S) : (sor (sand C (! A)) (snot A (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_76 (A B C D : S) : (sor (sand C (! B)) (snot B (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_77 (A B C D : S) : (sor (sand C (! D)) (snot C (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_78 (A B C D : S) : (sor (snot C (! D)) (sand C (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_79 (A B C D : S) : (snot (sor C (! A)) (snot (! A) C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_80 (A B C D : S) : (sxor (sor C (! A)) (snot (! A) C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_81 (A B C D : S) : (snot (sor C (! B)) (snot (! B) C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_82 (A B C D : S) : (sxor (sor C (! B)) (snot (! B) C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_83 (A B C D : S) : (snot (sor C (! D)) (snot (! C) D)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_84 (A B C D : S) : (sxor (sor C (! D)) (snot (! C) D)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_85 (A B C D : S) : (sxor (snot A (! C)) (sand C (! A))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_86 (A B C D : S) : (sxor (snot B (! C)) (sand C (! B))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_87 (A B C D : S) : (sxor (sand C (! A)) (snot A (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_88 (A B C D : S) : (sxor (sand C (! B)) (snot B (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_89 (A B C D : S) : (sxor (sand C (! D)) (snot C (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_90 (A B C D : S) : (sxor (snot C (! D)) (sand C (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_91 (A B C D : S) : (soutside (sor C (! A)) (snot (! A) C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_92 (A B C D : S) : (soutside (sor C (! B)) (snot (! B) C)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_93 (A B C D : S) : (soutside (sor C (! D)) (snot (! C) D)) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_94 (A B C D : S) : (sxor (sor (! A) (! C)) (sor A (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_95 (A B C D : S) : (sxor (sor (! B) (! C)) (sor B (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_96 (A B C D : S) : (sxor (sor (! C) (! D)) (sor D (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_97 (A B C D : S) : (sxor (sor A (! C)) (sor (! A) (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_98 (A B C D : S) : (sxor (sor B (! C)) (sor (! B) (! C))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_C_99 (A B C D : S) : (sxor (sor D (! C)) (sor (! C) (! D))) = C := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_0 (A B C D : S) : (snot D (! D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_1 (A B C D : S) : (soutside D (! D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_2 (A B C D : S) : (sor (sand A (! A)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_3 (A B C D : S) : (sor (snot A (! D)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_4 (A B C D : S) : (sand (sor A (! A)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_5 (A B C D : S) : (sor (snot B (! D)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_6 (A B C D : S) : (sor (snot C (! D)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_7 (A B C D : S) : (sor (sand D (! A)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_8 (A B C D : S) : (sor (sand D (! B)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_9 (A B C D : S) : (sor (sand D (! C)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_10 (A B C D : S) : (sand (sor D (! A)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_11 (A B C D : S) : (sand (sor D (! B)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_12 (A B C D : S) : (sand (sor D (! C)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_13 (A B C D : S) : (sand D (sor A (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_14 (A B C D : S) : (sand D (sor D (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_15 (A B C D : S) : (sand D (sor D (! B))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_16 (A B C D : S) : (sand D (sor D (! C))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_17 (A B C D : S) : (sor D (sand A (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_18 (A B C D : S) : (sor D (snot A (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_19 (A B C D : S) : (sor D (snot B (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_20 (A B C D : S) : (sor D (snot C (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_21 (A B C D : S) : (sor D (sand D (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_22 (A B C D : S) : (sor D (sand D (! B))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_23 (A B C D : S) : (sor D (sand D (! C))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_24 (A B C D : S) : (sxor (sand A (! A)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_25 (A B C D : S) : (snot D (snot (! A) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_26 (A B C D : S) : (snot D (snot (! B) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_27 (A B C D : S) : (snot D (snot (! C) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_28 (A B C D : S) : (snot D (sand A (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_29 (A B C D : S) : (snot D (sand A (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_30 (A B C D : S) : (snot D (sand B (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_31 (A B C D : S) : (snot D (sand C (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_32 (A B C D : S) : (sxor D (sand A (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_33 (A B C D : S) : (sinside (sor A (! A)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_34 (A B C D : S) : (sinside (sor D (! A)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_35 (A B C D : S) : (sinside (sor D (! B)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_36 (A B C D : S) : (sinside (sor D (! C)) D) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_37 (A B C D : S) : (sinside D (sor A (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_38 (A B C D : S) : (sinside D (sor D (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_39 (A B C D : S) : (sinside D (sor D (! B))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_40 (A B C D : S) : (sinside D (sor D (! C))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_41 (A B C D : S) : (soutside D (snot (! A) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_42 (A B C D : S) : (soutside D (snot (! B) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_43 (A B C D : S) : (soutside D (snot (! C) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_44 (A B C D : S) : (soutside D (sand A (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_45 (A B C D : S) : (soutside D (sand A (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_46 (A B C D : S) : (soutside D (sand B (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_47 (A B C D : S) : (soutside D (sand C (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_48 (A B C D : S) : (snot (sor A (! A)) (! D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_49 (A B C D : S) : (sxor (sor A (! A)) (! D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_50 (A B C D : S) : (snot (sor D (! A)) (! D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_51 (A B C D : S) : (snot (sor D (! B)) (! D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_52 (A B C D : S) : (snot (sor D (! C)) (! D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_53 (A B C D : S) : (sxor (! D) (sor A (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_54 (A B C D : S) : (sxor (sxor (! A) (! D)) A) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_55 (A B C D : S) : (sxor (sxor (! B) (! D)) B) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_56 (A B C D : S) : (sxor (sxor (! C) (! D)) C) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_57 (A B C D : S) : (sxor (sxor A (! D)) (! A)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_58 (A B C D : S) : (sxor (sxor B (! D)) (! B)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_59 (A B C D : S) : (sxor (sxor C (! D)) (! C)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_60 (A B C D : S) : (sxor (! A) (sxor A (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_61 (A B C D : S) : (sxor (! B) (sxor B (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_62 (A B C D : S) : (sxor (! C) (sxor C (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_63 (A B C D : S) : (sxor A (sxor (! A) (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_64 (A B C D : S) : (sxor B (sxor (! B) (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_65 (A B C D : S) : (sxor C (sxor (! C) (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_66 (A B C D : S) : (soutside (sor A (! A)) (! D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_67 (A B C D : S) : (soutside (sor D (! A)) (! D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_68 (A B C D : S) : (soutside (sor D (! B)) (! D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_69 (A B C D : S) : (soutside (sor D (! C)) (! D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_70 (A B C D : S) : (sxor (snot (! A) D) (sor D (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_71 (A B C D : S) : (sxor (snot (! B) D) (sor D (! B))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_72 (A B C D : S) : (sxor (snot (! C) D) (sor D (! C))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_73 (A B C D : S) : (sor (snot A (! D)) (sand D (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_74 (A B C D : S) : (sor (snot B (! D)) (sand D (! B))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_75 (A B C D : S) : (sor (snot C (! D)) (sand D (! C))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_76 (A B C D : S) : (sor (sand D (! A)) (snot A (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_77 (A B C D : S) : (sor (sand D (! B)) (snot B (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_78 (A B C D : S) : (sor (sand D (! C)) (snot C (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_79 (A B C D : S) : (snot (sor D (! A)) (snot (! A) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_80 (A B C D : S) : (sxor (sor D (! A)) (snot (! A) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_81 (A B C D : S) : (snot (sor D (! B)) (snot (! B) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_82 (A B C D : S) : (sxor (sor D (! B)) (snot (! B) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_83 (A B C D : S) : (snot (sor D (! C)) (snot (! C) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_84 (A B C D : S) : (sxor (sor D (! C)) (snot (! C) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_85 (A B C D : S) : (sxor (snot A (! D)) (sand D (! A))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_86 (A B C D : S) : (sxor (snot B (! D)) (sand D (! B))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_87 (A B C D : S) : (sxor (snot C (! D)) (sand D (! C))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_88 (A B C D : S) : (sxor (sand D (! A)) (snot A (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_89 (A B C D : S) : (sxor (sand D (! B)) (snot B (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_90 (A B C D : S) : (sxor (sand D (! C)) (snot C (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_91 (A B C D : S) : (soutside (sor D (! A)) (snot (! A) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_92 (A B C D : S) : (soutside (sor D (! B)) (snot (! B) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_93 (A B C D : S) : (soutside (sor D (! C)) (snot (! C) D)) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_94 (A B C D : S) : (sxor (sor (! A) (! D)) (sor A (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_95 (A B C D : S) : (sxor (sor (! B) (! D)) (sor B (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_96 (A B C D : S) : (sxor (sor (! C) (! D)) (sor C (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_97 (A B C D : S) : (sxor (sor A (! D)) (sor (! A) (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_98 (A B C D : S) : (sxor (sor B (! D)) (sor (! B) (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl
theorem e4_D_99 (A B C D : S) : (sxor (sor C (! D)) (sor (! C) (! D))) = D := by cases A <;> cases B <;> cases C <;> cases D <;> rfl

end VLSI.RelationsEquiv