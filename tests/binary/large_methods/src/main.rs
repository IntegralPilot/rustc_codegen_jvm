#[derive(Clone, Copy)]
struct ConstantEntry {
    value: u32,
    enabled: bool,
}

static LARGE_CONSTANT_TABLE: [ConstantEntry; 17_000] = [
    ConstantEntry {
        value: 37,
        enabled: true,
    };
    17_000
];

macro_rules! repeat_2 {
    ($($body:tt)*) => {
        $($body)*
        $($body)*
    };
}

macro_rules! repeat_4 {
    ($($body:tt)*) => {
        repeat_2! { $($body)* }
        repeat_2! { $($body)* }
    };
}

macro_rules! repeat_8 {
    ($($body:tt)*) => {
        repeat_4! { $($body)* }
        repeat_4! { $($body)* }
    };
}

macro_rules! repeat_16 {
    ($($body:tt)*) => {
        repeat_8! { $($body)* }
        repeat_8! { $($body)* }
    };
}

macro_rules! repeat_32 {
    ($($body:tt)*) => {
        repeat_16! { $($body)* }
        repeat_16! { $($body)* }
    };
}

macro_rules! repeat_64 {
    ($($body:tt)*) => {
        repeat_32! { $($body)* }
        repeat_32! { $($body)* }
    };
}

macro_rules! repeat_128 {
    ($($body:tt)*) => {
        repeat_64! { $($body)* }
        repeat_64! { $($body)* }
    };
}

macro_rules! repeat_256 {
    ($($body:tt)*) => {
        repeat_128! { $($body)* }
        repeat_128! { $($body)* }
    };
}

macro_rules! repeat_512 {
    ($($body:tt)*) => {
        repeat_256! { $($body)* }
        repeat_256! { $($body)* }
    };
}

macro_rules! repeat_1024 {
    ($($body:tt)*) => {
        repeat_512! { $($body)* }
        repeat_512! { $($body)* }
    };
}

macro_rules! repeat_2048 {
    ($($body:tt)*) => {
        repeat_1024! { $($body)* }
        repeat_1024! { $($body)* }
    };
}

#[inline(never)]
fn outlined_with_many_live_values(seed: u64) -> u64 {
    let value_0 = seed.wrapping_add(0);
    let value_1 = seed.wrapping_add(1);
    let value_2 = seed.wrapping_add(2);
    let value_3 = seed.wrapping_add(3);
    let value_4 = seed.wrapping_add(4);
    let value_5 = seed.wrapping_add(5);
    let value_6 = seed.wrapping_add(6);
    let value_7 = seed.wrapping_add(7);
    let value_8 = seed.wrapping_add(8);
    let value_9 = seed.wrapping_add(9);
    let value_10 = seed.wrapping_add(10);
    let value_11 = seed.wrapping_add(11);
    let value_12 = seed.wrapping_add(12);
    let value_13 = seed.wrapping_add(13);
    let value_14 = seed.wrapping_add(14);
    let value_15 = seed.wrapping_add(15);
    let value_16 = seed.wrapping_add(16);
    let value_17 = seed.wrapping_add(17);
    let value_18 = seed.wrapping_add(18);
    let value_19 = seed.wrapping_add(19);
    let value_20 = seed.wrapping_add(20);
    let value_21 = seed.wrapping_add(21);
    let value_22 = seed.wrapping_add(22);
    let value_23 = seed.wrapping_add(23);
    let value_24 = seed.wrapping_add(24);
    let value_25 = seed.wrapping_add(25);
    let value_26 = seed.wrapping_add(26);
    let value_27 = seed.wrapping_add(27);
    let value_28 = seed.wrapping_add(28);
    let value_29 = seed.wrapping_add(29);
    let value_30 = seed.wrapping_add(30);
    let value_31 = seed.wrapping_add(31);
    let value_32 = seed.wrapping_add(32);
    let value_33 = seed.wrapping_add(33);
    let value_34 = seed.wrapping_add(34);
    let value_35 = seed.wrapping_add(35);
    let value_36 = seed.wrapping_add(36);
    let value_37 = seed.wrapping_add(37);
    let value_38 = seed.wrapping_add(38);
    let value_39 = seed.wrapping_add(39);
    let value_40 = seed.wrapping_add(40);
    let value_41 = seed.wrapping_add(41);
    let value_42 = seed.wrapping_add(42);
    let value_43 = seed.wrapping_add(43);
    let value_44 = seed.wrapping_add(44);
    let value_45 = seed.wrapping_add(45);
    let value_46 = seed.wrapping_add(46);
    let value_47 = seed.wrapping_add(47);
    let value_48 = seed.wrapping_add(48);
    let value_49 = seed.wrapping_add(49);
    let value_50 = seed.wrapping_add(50);
    let value_51 = seed.wrapping_add(51);
    let value_52 = seed.wrapping_add(52);
    let value_53 = seed.wrapping_add(53);
    let value_54 = seed.wrapping_add(54);
    let value_55 = seed.wrapping_add(55);
    let value_56 = seed.wrapping_add(56);
    let value_57 = seed.wrapping_add(57);
    let value_58 = seed.wrapping_add(58);
    let value_59 = seed.wrapping_add(59);
    let value_60 = seed.wrapping_add(60);
    let value_61 = seed.wrapping_add(61);
    let value_62 = seed.wrapping_add(62);
    let value_63 = seed.wrapping_add(63);
    let value_64 = seed.wrapping_add(64);
    let value_65 = seed.wrapping_add(65);
    let value_66 = seed.wrapping_add(66);
    let value_67 = seed.wrapping_add(67);
    let value_68 = seed.wrapping_add(68);
    let value_69 = seed.wrapping_add(69);
    let value_70 = seed.wrapping_add(70);
    let value_71 = seed.wrapping_add(71);
    let value_72 = seed.wrapping_add(72);
    let value_73 = seed.wrapping_add(73);
    let value_74 = seed.wrapping_add(74);
    let value_75 = seed.wrapping_add(75);
    let value_76 = seed.wrapping_add(76);
    let value_77 = seed.wrapping_add(77);
    let value_78 = seed.wrapping_add(78);
    let value_79 = seed.wrapping_add(79);
    let value_80 = seed.wrapping_add(80);
    let value_81 = seed.wrapping_add(81);
    let value_82 = seed.wrapping_add(82);
    let value_83 = seed.wrapping_add(83);
    let value_84 = seed.wrapping_add(84);
    let value_85 = seed.wrapping_add(85);
    let value_86 = seed.wrapping_add(86);
    let value_87 = seed.wrapping_add(87);
    let value_88 = seed.wrapping_add(88);
    let value_89 = seed.wrapping_add(89);
    let value_90 = seed.wrapping_add(90);
    let value_91 = seed.wrapping_add(91);
    let value_92 = seed.wrapping_add(92);
    let value_93 = seed.wrapping_add(93);
    let value_94 = seed.wrapping_add(94);
    let value_95 = seed.wrapping_add(95);
    let value_96 = seed.wrapping_add(96);
    let value_97 = seed.wrapping_add(97);
    let value_98 = seed.wrapping_add(98);
    let value_99 = seed.wrapping_add(99);
    let value_100 = seed.wrapping_add(100);
    let value_101 = seed.wrapping_add(101);
    let value_102 = seed.wrapping_add(102);
    let value_103 = seed.wrapping_add(103);
    let value_104 = seed.wrapping_add(104);
    let value_105 = seed.wrapping_add(105);
    let value_106 = seed.wrapping_add(106);
    let value_107 = seed.wrapping_add(107);
    let value_108 = seed.wrapping_add(108);
    let value_109 = seed.wrapping_add(109);
    let value_110 = seed.wrapping_add(110);
    let value_111 = seed.wrapping_add(111);
    let value_112 = seed.wrapping_add(112);
    let value_113 = seed.wrapping_add(113);
    let value_114 = seed.wrapping_add(114);
    let value_115 = seed.wrapping_add(115);
    let value_116 = seed.wrapping_add(116);
    let value_117 = seed.wrapping_add(117);
    let value_118 = seed.wrapping_add(118);
    let value_119 = seed.wrapping_add(119);
    let value_120 = seed.wrapping_add(120);
    let value_121 = seed.wrapping_add(121);
    let value_122 = seed.wrapping_add(122);
    let value_123 = seed.wrapping_add(123);
    let value_124 = seed.wrapping_add(124);
    let value_125 = seed.wrapping_add(125);
    let value_126 = seed.wrapping_add(126);
    let value_127 = seed.wrapping_add(127);
    let value_128 = seed.wrapping_add(128);
    let value_129 = seed.wrapping_add(129);
    let value_130 = seed.wrapping_add(130);
    let value_131 = seed.wrapping_add(131);
    let value_132 = seed.wrapping_add(132);
    let value_133 = seed.wrapping_add(133);
    let value_134 = seed.wrapping_add(134);
    let value_135 = seed.wrapping_add(135);
    let value_136 = seed.wrapping_add(136);
    let value_137 = seed.wrapping_add(137);
    let value_138 = seed.wrapping_add(138);
    let value_139 = seed.wrapping_add(139);
    let value_140 = seed.wrapping_add(140);
    let value_141 = seed.wrapping_add(141);
    let value_142 = seed.wrapping_add(142);
    let value_143 = seed.wrapping_add(143);
    let value_144 = seed.wrapping_add(144);
    let value_145 = seed.wrapping_add(145);
    let value_146 = seed.wrapping_add(146);
    let value_147 = seed.wrapping_add(147);
    let value_148 = seed.wrapping_add(148);
    let value_149 = seed.wrapping_add(149);
    let value_150 = seed.wrapping_add(150);
    let value_151 = seed.wrapping_add(151);
    let value_152 = seed.wrapping_add(152);
    let value_153 = seed.wrapping_add(153);
    let value_154 = seed.wrapping_add(154);
    let value_155 = seed.wrapping_add(155);
    let value_156 = seed.wrapping_add(156);
    let value_157 = seed.wrapping_add(157);
    let value_158 = seed.wrapping_add(158);
    let value_159 = seed.wrapping_add(159);
    let value_160 = seed.wrapping_add(160);
    let value_161 = seed.wrapping_add(161);
    let value_162 = seed.wrapping_add(162);
    let value_163 = seed.wrapping_add(163);
    let value_164 = seed.wrapping_add(164);
    let value_165 = seed.wrapping_add(165);
    let value_166 = seed.wrapping_add(166);
    let value_167 = seed.wrapping_add(167);
    let value_168 = seed.wrapping_add(168);
    let value_169 = seed.wrapping_add(169);
    let value_170 = seed.wrapping_add(170);
    let value_171 = seed.wrapping_add(171);
    let value_172 = seed.wrapping_add(172);
    let value_173 = seed.wrapping_add(173);
    let value_174 = seed.wrapping_add(174);
    let value_175 = seed.wrapping_add(175);
    let value_176 = seed.wrapping_add(176);
    let value_177 = seed.wrapping_add(177);
    let value_178 = seed.wrapping_add(178);
    let value_179 = seed.wrapping_add(179);
    let value_180 = seed.wrapping_add(180);
    let value_181 = seed.wrapping_add(181);
    let value_182 = seed.wrapping_add(182);
    let value_183 = seed.wrapping_add(183);
    let value_184 = seed.wrapping_add(184);
    let value_185 = seed.wrapping_add(185);
    let value_186 = seed.wrapping_add(186);
    let value_187 = seed.wrapping_add(187);
    let value_188 = seed.wrapping_add(188);
    let value_189 = seed.wrapping_add(189);
    let value_190 = seed.wrapping_add(190);
    let value_191 = seed.wrapping_add(191);
    let value_192 = seed.wrapping_add(192);
    let value_193 = seed.wrapping_add(193);
    let value_194 = seed.wrapping_add(194);
    let value_195 = seed.wrapping_add(195);
    let value_196 = seed.wrapping_add(196);
    let value_197 = seed.wrapping_add(197);
    let value_198 = seed.wrapping_add(198);
    let value_199 = seed.wrapping_add(199);
    let value_200 = seed.wrapping_add(200);
    let value_201 = seed.wrapping_add(201);
    let value_202 = seed.wrapping_add(202);
    let value_203 = seed.wrapping_add(203);
    let value_204 = seed.wrapping_add(204);
    let value_205 = seed.wrapping_add(205);
    let value_206 = seed.wrapping_add(206);
    let value_207 = seed.wrapping_add(207);
    let value_208 = seed.wrapping_add(208);
    let value_209 = seed.wrapping_add(209);
    let value_210 = seed.wrapping_add(210);
    let value_211 = seed.wrapping_add(211);
    let value_212 = seed.wrapping_add(212);
    let value_213 = seed.wrapping_add(213);
    let value_214 = seed.wrapping_add(214);
    let value_215 = seed.wrapping_add(215);
    let value_216 = seed.wrapping_add(216);
    let value_217 = seed.wrapping_add(217);
    let value_218 = seed.wrapping_add(218);
    let value_219 = seed.wrapping_add(219);
    let value_220 = seed.wrapping_add(220);
    let value_221 = seed.wrapping_add(221);
    let value_222 = seed.wrapping_add(222);
    let value_223 = seed.wrapping_add(223);
    let value_224 = seed.wrapping_add(224);
    let value_225 = seed.wrapping_add(225);
    let value_226 = seed.wrapping_add(226);
    let value_227 = seed.wrapping_add(227);
    let value_228 = seed.wrapping_add(228);
    let value_229 = seed.wrapping_add(229);
    let value_230 = seed.wrapping_add(230);
    let value_231 = seed.wrapping_add(231);
    let value_232 = seed.wrapping_add(232);
    let value_233 = seed.wrapping_add(233);
    let value_234 = seed.wrapping_add(234);
    let value_235 = seed.wrapping_add(235);
    let value_236 = seed.wrapping_add(236);
    let value_237 = seed.wrapping_add(237);
    let value_238 = seed.wrapping_add(238);
    let value_239 = seed.wrapping_add(239);
    let value_240 = seed.wrapping_add(240);
    let value_241 = seed.wrapping_add(241);
    let value_242 = seed.wrapping_add(242);
    let value_243 = seed.wrapping_add(243);
    let value_244 = seed.wrapping_add(244);
    let value_245 = seed.wrapping_add(245);
    let value_246 = seed.wrapping_add(246);
    let value_247 = seed.wrapping_add(247);
    let value_248 = seed.wrapping_add(248);
    let value_249 = seed.wrapping_add(249);
    let value_250 = seed.wrapping_add(250);
    let value_251 = seed.wrapping_add(251);
    let value_252 = seed.wrapping_add(252);
    let value_253 = seed.wrapping_add(253);
    let value_254 = seed.wrapping_add(254);
    let value_255 = seed.wrapping_add(255);
    let value_256 = seed.wrapping_add(256);
    let value_257 = seed.wrapping_add(257);
    let value_258 = seed.wrapping_add(258);
    let value_259 = seed.wrapping_add(259);
    let value_260 = seed.wrapping_add(260);
    let value_261 = seed.wrapping_add(261);
    let value_262 = seed.wrapping_add(262);
    let value_263 = seed.wrapping_add(263);
    let value_264 = seed.wrapping_add(264);
    let value_265 = seed.wrapping_add(265);
    let value_266 = seed.wrapping_add(266);
    let value_267 = seed.wrapping_add(267);
    let value_268 = seed.wrapping_add(268);
    let value_269 = seed.wrapping_add(269);

    let mut acc = 0u64;
    repeat_2048! {
        acc = acc.wrapping_add(1);
    }

    acc ^= value_0;
    acc ^= value_1;
    acc ^= value_2;
    acc ^= value_3;
    acc ^= value_4;
    acc ^= value_5;
    acc ^= value_6;
    acc ^= value_7;
    acc ^= value_8;
    acc ^= value_9;
    acc ^= value_10;
    acc ^= value_11;
    acc ^= value_12;
    acc ^= value_13;
    acc ^= value_14;
    acc ^= value_15;
    acc ^= value_16;
    acc ^= value_17;
    acc ^= value_18;
    acc ^= value_19;
    acc ^= value_20;
    acc ^= value_21;
    acc ^= value_22;
    acc ^= value_23;
    acc ^= value_24;
    acc ^= value_25;
    acc ^= value_26;
    acc ^= value_27;
    acc ^= value_28;
    acc ^= value_29;
    acc ^= value_30;
    acc ^= value_31;
    acc ^= value_32;
    acc ^= value_33;
    acc ^= value_34;
    acc ^= value_35;
    acc ^= value_36;
    acc ^= value_37;
    acc ^= value_38;
    acc ^= value_39;
    acc ^= value_40;
    acc ^= value_41;
    acc ^= value_42;
    acc ^= value_43;
    acc ^= value_44;
    acc ^= value_45;
    acc ^= value_46;
    acc ^= value_47;
    acc ^= value_48;
    acc ^= value_49;
    acc ^= value_50;
    acc ^= value_51;
    acc ^= value_52;
    acc ^= value_53;
    acc ^= value_54;
    acc ^= value_55;
    acc ^= value_56;
    acc ^= value_57;
    acc ^= value_58;
    acc ^= value_59;
    acc ^= value_60;
    acc ^= value_61;
    acc ^= value_62;
    acc ^= value_63;
    acc ^= value_64;
    acc ^= value_65;
    acc ^= value_66;
    acc ^= value_67;
    acc ^= value_68;
    acc ^= value_69;
    acc ^= value_70;
    acc ^= value_71;
    acc ^= value_72;
    acc ^= value_73;
    acc ^= value_74;
    acc ^= value_75;
    acc ^= value_76;
    acc ^= value_77;
    acc ^= value_78;
    acc ^= value_79;
    acc ^= value_80;
    acc ^= value_81;
    acc ^= value_82;
    acc ^= value_83;
    acc ^= value_84;
    acc ^= value_85;
    acc ^= value_86;
    acc ^= value_87;
    acc ^= value_88;
    acc ^= value_89;
    acc ^= value_90;
    acc ^= value_91;
    acc ^= value_92;
    acc ^= value_93;
    acc ^= value_94;
    acc ^= value_95;
    acc ^= value_96;
    acc ^= value_97;
    acc ^= value_98;
    acc ^= value_99;
    acc ^= value_100;
    acc ^= value_101;
    acc ^= value_102;
    acc ^= value_103;
    acc ^= value_104;
    acc ^= value_105;
    acc ^= value_106;
    acc ^= value_107;
    acc ^= value_108;
    acc ^= value_109;
    acc ^= value_110;
    acc ^= value_111;
    acc ^= value_112;
    acc ^= value_113;
    acc ^= value_114;
    acc ^= value_115;
    acc ^= value_116;
    acc ^= value_117;
    acc ^= value_118;
    acc ^= value_119;
    acc ^= value_120;
    acc ^= value_121;
    acc ^= value_122;
    acc ^= value_123;
    acc ^= value_124;
    acc ^= value_125;
    acc ^= value_126;
    acc ^= value_127;
    acc ^= value_128;
    acc ^= value_129;
    acc ^= value_130;
    acc ^= value_131;
    acc ^= value_132;
    acc ^= value_133;
    acc ^= value_134;
    acc ^= value_135;
    acc ^= value_136;
    acc ^= value_137;
    acc ^= value_138;
    acc ^= value_139;
    acc ^= value_140;
    acc ^= value_141;
    acc ^= value_142;
    acc ^= value_143;
    acc ^= value_144;
    acc ^= value_145;
    acc ^= value_146;
    acc ^= value_147;
    acc ^= value_148;
    acc ^= value_149;
    acc ^= value_150;
    acc ^= value_151;
    acc ^= value_152;
    acc ^= value_153;
    acc ^= value_154;
    acc ^= value_155;
    acc ^= value_156;
    acc ^= value_157;
    acc ^= value_158;
    acc ^= value_159;
    acc ^= value_160;
    acc ^= value_161;
    acc ^= value_162;
    acc ^= value_163;
    acc ^= value_164;
    acc ^= value_165;
    acc ^= value_166;
    acc ^= value_167;
    acc ^= value_168;
    acc ^= value_169;
    acc ^= value_170;
    acc ^= value_171;
    acc ^= value_172;
    acc ^= value_173;
    acc ^= value_174;
    acc ^= value_175;
    acc ^= value_176;
    acc ^= value_177;
    acc ^= value_178;
    acc ^= value_179;
    acc ^= value_180;
    acc ^= value_181;
    acc ^= value_182;
    acc ^= value_183;
    acc ^= value_184;
    acc ^= value_185;
    acc ^= value_186;
    acc ^= value_187;
    acc ^= value_188;
    acc ^= value_189;
    acc ^= value_190;
    acc ^= value_191;
    acc ^= value_192;
    acc ^= value_193;
    acc ^= value_194;
    acc ^= value_195;
    acc ^= value_196;
    acc ^= value_197;
    acc ^= value_198;
    acc ^= value_199;
    acc ^= value_200;
    acc ^= value_201;
    acc ^= value_202;
    acc ^= value_203;
    acc ^= value_204;
    acc ^= value_205;
    acc ^= value_206;
    acc ^= value_207;
    acc ^= value_208;
    acc ^= value_209;
    acc ^= value_210;
    acc ^= value_211;
    acc ^= value_212;
    acc ^= value_213;
    acc ^= value_214;
    acc ^= value_215;
    acc ^= value_216;
    acc ^= value_217;
    acc ^= value_218;
    acc ^= value_219;
    acc ^= value_220;
    acc ^= value_221;
    acc ^= value_222;
    acc ^= value_223;
    acc ^= value_224;
    acc ^= value_225;
    acc ^= value_226;
    acc ^= value_227;
    acc ^= value_228;
    acc ^= value_229;
    acc ^= value_230;
    acc ^= value_231;
    acc ^= value_232;
    acc ^= value_233;
    acc ^= value_234;
    acc ^= value_235;
    acc ^= value_236;
    acc ^= value_237;
    acc ^= value_238;
    acc ^= value_239;
    acc ^= value_240;
    acc ^= value_241;
    acc ^= value_242;
    acc ^= value_243;
    acc ^= value_244;
    acc ^= value_245;
    acc ^= value_246;
    acc ^= value_247;
    acc ^= value_248;
    acc ^= value_249;
    acc ^= value_250;
    acc ^= value_251;
    acc ^= value_252;
    acc ^= value_253;
    acc ^= value_254;
    acc ^= value_255;
    acc ^= value_256;
    acc ^= value_257;
    acc ^= value_258;
    acc ^= value_259;
    acc ^= value_260;
    acc ^= value_261;
    acc ^= value_262;
    acc ^= value_263;
    acc ^= value_264;
    acc ^= value_265;
    acc ^= value_266;
    acc ^= value_267;
    acc ^= value_268;
    acc ^= value_269;
    acc
}

fn reference(seed: u64) -> u64 {
    let mut expected = 2048u64;
    for index in 0..270 {
        expected ^= seed.wrapping_add(index);
    }
    expected
}

fn main() {
    assert_eq!(LARGE_CONSTANT_TABLE.len(), 17_000);
    assert_eq!(LARGE_CONSTANT_TABLE[0].value, 37);
    assert!(LARGE_CONSTANT_TABLE[16_999].enabled);

    for seed in [0, 1, 7, u32::MAX as u64, u64::MAX - 300] {
        assert_eq!(outlined_with_many_live_values(seed), reference(seed));
    }
}
