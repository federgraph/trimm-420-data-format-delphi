unit RiggVar.FB.DefConst;

(*
-
-     F            info: http://wwww.riggvar.de
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) RiggVar Software UG (haftungsbeschränkt)
-
*)

interface

uses
  System.Classes;

const
  fmdScene = 1;
  fmdGraph = 2;
  fmdFigure = 3;
  fmdPlot = 4;
  fmdColor = 5;
  fmdSample = 6;
  fmdHub = 7;
  fmdPlusCap = 8;
  fmdMinusCap = 9;
  fmdOpacity = 10;
  fmdBitmap = 11;
  fmdMeshSize = 12;
  fmdParam = 13;
  fmdParamValue = 14;
  fmdAction = 15;
  fmdTX = 16;
  fmdTY = 17;
  fmdRX = 18;
  fmdRY = 19;
  fmdRZ = 20;
  fmdCZ = 21;
  fmdX1 = 22;
  fmdY1 = 23;
  fmdX2 = 24;
  fmdY2 = 25;
  fmdX3 = 26;
  fmdY3 = 27;
  fmdAnimNewStory = 28;
  fmdAnimNewEntry = 29;
  fmdAnimParam = 30;
  fmdAnimStartValue = 31;
  fmdAnimStopValue = 32;
  fmdAnimGO = 33;
  fmdAnimAT = 34;
  fmdAnimIT = 35;
  fmdAnimDU = 36;
  fmdAnimAR = 37;
  fmdAnimLP = 38;
  fmdAnimFC = 39;

//  DisplayConst = class
//  public const
    Display2D = 0;
    Display3D = 1;
    Display32 = 2;
    Display33 = 3;
    Display64 = 4;
    Display66 = 5;
    Display00 = 6;

    DisloF = 0;
    DisloL = 1;
    DisloR = 2;
    DisloT = 3;
    DisloB = 4;

    DisloTL = 5;
    DisloTR = 6;
    DisloBL = 7;
    DisloBR = 8;

    DisloHideM = 9;
    DisloHideL = 10;
    DisloHideR = 11;
    DisloHideT = 12;
    DisloHideB = 13;

    DisloHideTL = 14;
    DisloHideTR = 15;
    DisloHideBL = 16;
    DisloHideBR = 17;
//  end;

//  FormatStrings = class
//  public const
    //Delphi code format strings
    dsg = '%s := %g;';
    dsd = '%s := %d;';
    dss = '%s := %s;';
    dsf = '%s := %2.2f;';

    dbg = '%d := %g;';
    dbd = '%d := %d;';
    dbs = '%d := %s;';
    dbf = '%d := %2.2f;';

    //Java code format strings
    jsg = '%s = %g;';
    jsd = '%s = %d;';
    jss = '%s = %s;';
    jsf = '%s = %2.2f;';

    jbg = '%d = %g;';
    jbd = '%d = %d;';
    jbs = '%d = %s;';
    jbf = '%d = %2.2f;';

    //normal properties file format strings
    fsg = '%s=%g';
    fsd = '%s=%d';
    fss = '%s=%s';
    fsf = '%s=%2.2f';

    fbg = '%d=%g';
    fbd = '%d=%d';
    fbs = '%d=%s';
    fbf = '%d=%2.2f';
//  end;

//  DataConst = class
//  public const
    cVersion = 'version';
    nVersion=0;

    cx1 = 'x1';
    cx2 = 'x2';
    cx3 = 'x3';
    cx4 = 'x4';
    nx1 = 1;
    nx2 = 2;
    nx3 = 3;
    nx4 = 4;

    cy1 = 'y1';
    cy2 = 'y2';
    cy3 = 'y3';
    cy4 = 'y4';
    ny1 = 5;
    ny2 = 6;
    ny3 = 7;
    ny4 = 8;

    cl1 = 'l1';
    cl2 = 'l2';
    cl3 = 'l3';
    cl4 = 'l4';
    nl1 = 9;
    nl2 = 10;
    nl3 = 11;
    nl4 = 12;

    cz1 = 'z1';
    cz2 = 'z2';
    cz3 = 'z3';
    cz4 = 'z4';
    nz1 = 13;
    nz2 = 14;
    nz3 = 15;
    nz4 = 16;

    ck1 = 'k1';
    ck2 = 'k2';
    ck3 = 'k3';
    ck4 = 'k4';
    nk1 = 17;
    nk2 = 18;
    nk3 = 19;
    nk4 = 20;

    cScene = 'Scene';
    cGraph = 'Graph';
    cPlot = 'Plot';
    cFigure = 'Figure';
    cBitmap = 'Bitmap';
    cColor = 'Color';
    nScene = 21;
    nGraph = 22;
    nPlot = 23;
    nFigure = 24;
    nBitmap = 25;
    nColor = 26;

    cOffsetX = 'OffsetX';
    cOffsetY = 'OffsetY';
    cOffsetZ = 'OffsetZ';
    nOffsetX = 27;
    nOffsetY = 28;
    nOffsetZ = 29;

    cResolution = 'Resolution';
    cMeshSize = 'MeshSize';
    cGain = 'Gain';
    cLimit = 'Limit';
    cRange = 'Range';
    cParam = 'Param';
    nResolution = 30;
    nMeshSize = 31;
    nGain = 32;
    nLimit = 33;
    nRange = 34;
    nParam = 35;

    cMinusCap = 'MinusCap';
    cPlusCap = 'PlusCap';
    cOpacity = 'Opacity';
    cBigmap = 'Bigmap';
    nMinusCap = 36;
    nPlusCap = 37;
    nOpacity = 38;
    nBigmap = 39;

    cAngleX = 'AngleX';
    cAngleY = 'AngleY';
    cAngleZ = 'AngleZ';
    cPosX = 'PosX';
    cPosY = 'PosY';
    cPosZ = 'PosZ';
    nAngleX = 40;
    nAngleY = 41;
    nAngleZ = 42;
    nPosX = 43;
    nPosY = 44;
    nPosZ = 45;

    cParamValue = 'ParamValue';
    cLevel = 'Level';
    cHub = 'Hub';
    cSample = 'Sample';
    cAction = 'Action';
    nParamValue = 46;
    nLevel = 47;
    nHub = 48;
    nSample = 49;
    nAction = 50;

    cRX = 'RX';
    cRY = 'RY';
    cRZ = 'RZ';
    nRX = 51;
    nRY = 52;
    nRZ = 53;

    cTX = 'TX';
    cTY = 'TY';
    cCZ = 'CZ';
    cPA = 'PA';
    nTX = 54;
    nTY = 55;
    nCZ = 56;
    nPA = 57;

    cAnimNewStory = 'NS';
    cAnimNewEntry = 'NE';
    cAnimGO = 'GO';
    cAnimParam = 'PA';
    cAnimStartValue = 'VA';
    cAnimStopValue = 'VE';
    cAnimAnimationType = 'AT';
    cAnimInterpolationType = 'IT';
    cAnimDuration = 'DU';
    cAnimAutoReverse = 'AR';
    cAnimLoop = 'LP';
    cAnimFromCurrent = 'FC';
    nAnimNewStory = 58;
    nAnimNewEntry = 59;
    nAnimGO = 60;
    nAnimParam = 61;
    nAnimStartValue = 62;
    nAnimStopValue = 63;
    nAnimAnimationType = 64;
    nAnimInterpolationType = 65;
    nAnimDuration = 66;
    nAnimAutoReverse = 67;
    nAnimLoop = 68;
    nAnimFromCurrent = 69;

    cQX = 'QX';
    cQY = 'QY';
    cQZ = 'QZ';
    cQW = 'QW';
    cQR = 'QR';
    nQX = 70;
    nQY = 71;
    nQZ = 72;
    nQW = 73;
    nQR = 74;

    cT1 = 'T1';
    cT2 = 'T2';
    cT3 = 'T3';
    cT4 = 'T4';
    nT1 = 75;
    nT2 = 76;
    nT3 = 77;
    nT4 = 78;

    cParallelX1 = 'pX1';
    cParallelX2 = 'pX2';
    cParallelX3 = 'pX3';
    cParallelX4 = 'pX4';
    nParallelX1 = 79;
    nParallelX2 = 80;
    nParallelX3 = 81;
    nParallelX4 = 82;

    cParallelY1 = 'pY1';
    cParallelY2 = 'pY2';
    cParallelY3 = 'pY3';
    cParallelY4 = 'pY4';
    nParallelY1 = 83;
    nParallelY2 = 84;
    nParallelY3 = 85;
    nParallelY4 = 86;

    cParallelZ = 'pZ';
    cParallelZ1 = 'pZ1';
    cParallelZ2 = 'pZ2';
    cParallelZ3 = 'pZ3';
    cParallelZ4 = 'pZ4';
    nParallelZ = 87;
    nParallelZ1 = 88;
    nParallelZ2 = 89;
    nParallelZ3 = 90;
    nParallelZ4 = 91;

    cParallelL= 'pL';
    cParallelL1 = 'pL1';
    cParallelL2 = 'pL2';
    cParallelL3 = 'pL3';
    cParallelL4 = 'pL4';
    nParallelL= 92;
    nParallelL1 = 93;
    nParallelL2 = 94;
    nParallelL3 = 95;
    nParallelL4 = 96;

    cParallelK= 'pK';
    cParallelK1 = 'pK1';
    cParallelK2 = 'pK2';
    cParallelK3 = 'pK3';
    cParallelK4 = 'pK4';
    nParallelK= 97;
    nParallelK1 = 98;
    nParallelK2 = 99;
    nParallelK3 = 100;
    nParallelK4 = 101;

    cParallelOX = 'pOX';
    cParallelOY = 'pOY';
    cParallelOZ = 'pOZ';
    nParallelOX = 102;
    nParallelOY = 103;
    nParallelOZ = 104;

    cParallelRX = 'pRX';
    cParallelRY = 'pRY';
    cParallelRZ = 'pRZ';
    cParallelCZ = 'pCZ';
    cParallelSW = 'pSW';
    cParallelBW = 'pBW';
    cParallelDW = 'pDW';
    nParallelRX = 105;
    nParallelRY = 106;
    nParallelRZ = 107;
    nParallelCZ = 108;
    nParallelSW = 109;
    nParallelBW = 110;
    nParallelDW = 111;

    cParallelT= 'pT';
    cParallelT1= 'pT1';
    cParallelT2= 'pT2';
    cParallelT3= 'pT3';
    cParallelT4= 'pT4';
    nParallelT= 112;
    nParallelT1= 113;
    nParallelT2= 114;
    nParallelT3= 115;
    nParallelT4= 116;

    cParallelGain= 'pGain';
    cParallelLimit= 'pLimit';
    cParallelRange= 'pRange';
    nParallelGain= 117;
    nParallelLimit= 118;
    nParallelRange= 119;

    cParallelSample = 'pS';
    cParallelHub = 'pH';
    cParallelColor = 'pC';
    nParallelSample = 120;
    nParallelHub = 121;
    nParallelColor = 122;

    cm1 = 'm1';
    cm2 = 'm2';
    cm3 = 'm3';
    cm4 = 'm4';
    nm1 = 123;
    nm2 = 124;
    nm3 = 125;
    nm4 = 126;

    cForceMode = 'ForceMode';
    cSolutionMode = 'SolutionMode';
    cPlotFigure = 'PlotFigure';
    cDrawFigure = 'DrawFigure';
    cVorzeichen = 'Sign';
    cSliceMode = 'SliceMode';
    cOrthoCamera = 'OrthoCamera';
    cOpenMesh = 'OpenMesh';
    cPolarMesh = 'PolarMesh';
    cLinearMesh = 'LinearMesh';
    cFuzzyMesh = 'FuzzyMesh';
    cFilterMesh = 'FilterMesh';
    cDim = 'Dim';
    cGleich = 'Gleich';
    cLinearForce = 'LinearForce';
    nForceMode = 127;
    nSolutionMode = 128;
    nPlotFigure = 129;
    nVorzeichen = 130;
    nSliceMode = 131;
    nOrthoCamera = 132;
    nOpenMesh = 133;
    nPolarMesh = 134;
    nLinearMesh = 135;
    nFuzzyMesh = 136;
    nFilterMesh = 137;
    nDim = 138;
    nGleich = 139;
    nLinearForce = 140;

    kmZugDruck = 0;
    kmZug = 1;
    kmDruck = 2;
    kmIndividual = 3;

    civ = 'iv';
    ciw = 'iw';
    cjv = 'jv';
    cjw = 'jw';
    niv = 141;
    niw = 142;
    njv = 143;
    njw = 144;

    cpx = 'px';
    cpy = 'py';
    cva = 'va';
    cnp = 'np';
    cfp = 'fp';
    npx = 145;
    npy = 146;
    nva = 147;
    nnp = 148;
    nfp = 149;

    cp0 = 'p0';
    cp1 = 'p1';
    cp2 = 'p2';
    cp3 = 'p3';
    cp4 = 'p4';
    cp5 = 'p5';
    cp6 = 'p6';
    cp7 = 'p7';
    cp8 = 'p8';
    cp9 = 'p9';
    np0 = 150;
    np1 = 151;
    np2 = 152;
    np3 = 153;
    np4 = 154;
    np5 = 155;
    np6 = 156;
    np7 = 157;
    np8 = 158;
    np9 = 159;

    cEulerX = 'eux';
    cEulerY = 'euy';
    cEulerZ = 'euz';
    nEulerX = 160;
    nEulerY = 161;
    nEulerZ = 162;

    cl1x = 'l1x';
    cl1y = 'l1y';
    cl1z = 'l1z';

    cl2x = 'l2x';
    cl2y = 'l2y';
    cl2z = 'l2z';

    cl3x = 'l3x';
    cl3y = 'l3y';
    cl3z = 'l3z';

    cl4x = 'l4x';
    cl4y = 'l4y';
    cl4z = 'l4z';
    clkw = 'kw';
    clmz = 'mz';

    cbpr = 'bpr';
    cbpx = 'bpx';
    cbpy = 'bpy';
    cbpa = 'bpa';
    cbs1 = 'bs1';
    cbs2 = 'bs2';
    cbpf = 'bcf';
    cbpd = 'bcd';
    cbpz = 'bcz';
    nbpr = 163;
    nbpx = 164;
    nbpy = 165;
    nbpa = 166;
    nbs1 = 167;
    nbs2 = 168;
    nbpf = 169;
    nbpd = 170;
    nbpz = 171;

    cshgr = 'shg';
    cshku = 'shk';
    cshcy = 'shc';
    cshdi = 'shd';
    cshlb = 'shb';
    cshll = 'shl';
    cshc1 = 'sh1';
    cshc2 = 'sh2';
    nshgr = 172;
    nshku = 173;
    nshcy = 174;
    nshdi = 175;
    nshlb = 176;
    nshll = 177;
    nshc1 = 178;
    nshc2 = 179;

    cnpx = 'npx';
    cnpy = 'npy';
    cpbz = 'pbz';
    cpbr = 'pbr';
    nnpx = 180;
    nnpy = 181;
    npbz = 182;
    npbr = 183;

    ccs1 = 'cs1';
    ccs2 = 'cs2';
    ncs1 = 184;
    ncs2 = 185;

    cogr = 'ogr';
    cozm = 'ozm';
    corx = 'orx';
    cory = 'ory';
    corz = 'orz';
    nogr = 186;
    nozm = 187;
    norx = 188;
    nory = 189;
    norz = 190;

    cParallelORX = 'pORX';
    cParallelORY = 'pORY';
    cParallelORZ = 'pORZ';
    nParallelORX = 191;
    nParallelORY = 192;
    nParallelORZ = 193;

    cParamBahnRadius = 'ParamBahnRadius';
    cParamBahnPositionX = 'ParamBahnPositionX';
    cParamBahnPositionY = 'ParamBahnPositionY';
    cParamBahnAngle = 'ParamBahnAngle';
    nParamBahnRadius = 194;
    nParamBahnPositionX = 195;
    nParamBahnPositionY = 196;
    nParamBahnAngle = 197;

    cParamBahnStrokeWidth1 = 'ParamBahnStrokeWidth1';
    cParamBahnStrokeWidth2 = 'ParamBahnStrokeWidth2';
    nParamBahnStrokeWidth1 = 198;
    nParamBahnStrokeWidth2 = 199;

    cParamBahnCylinderF = 'ParamBahnCylinderF';
    cParamBahnCylinderD = 'ParamBahnCylinderD';
    cParamBahnCylinderZ = 'ParamBahnCylinderZ';
    nParamBahnCylinderF = 200;
    nParamBahnCylinderD = 201;
    nParamBahnCylinderZ = 202;

    cShowGrid = 'ShowGrid';
    cShowKugel = 'ShowKugel';
    cShowCylinder = 'ShowCylinder';
    cShowDiameter = 'ShowDiameter';
    cShowLB = 'ShowLB';
    cShowLL = 'ShowLL';
    cShowLC1 = 'ShowLC1';
    cShowLC2 = 'ShowLC2';
    nShowGrid = 203;
    nShowKugel = 204;
    nShowCylinder = 205;
    nShowDiameter = 206;
    nShowLB = 207;
    nShowLL = 208;
    nShowLC1 = 209;
    nShowLC2 = 210;

    cNullpunktX = 'NullpunktX';
    cNullpunktY = 'NullpunktY';
    cPaintboxZoom = 'PaintboxZoom';
    cPaintboxRotation = 'PaintboxRotation';
    nNullpunktX = 211;
    nNullpunktY = 212;
    nPaintboxZoom = 213;
    nPaintboxRotation = 214;

    cColorScheme2D = 'ColorScheme2D';
    cColorScheme3D = 'ColorScheme3D';
    nColorScheme2D = 215;
    nColorScheme3D = 216;

    cOrthoGraph = 'OrthoGraph';
    cOrthoZoom = 'OrthoZoom';
    cOrthoRotX = 'OrthoRotX';
    cOrthoRotY = 'OrthoRotY';
    cOrthoRotZ = 'OrthoRotZ';
    cOrthoDefX = 'OrthoDefX';
    cOrthoDefY = 'OrthoDefY';
    cOrthoDefZ = 'OrthoDefZ';
    nOrthoGraph = 217;
    nOrthoZoom = 218;
    nOrthoRotX = 219;
    nOrthoRotY = 220;
    nOrthoRotZ = 221;
    nOrthoDefX = 222;
    nOrthoDefY = 223;
    nOrthoDefZ = 224;

    cLux = 'Lux';
    nLux = 225;

    cLux1  = 'Lux1';
    cLux1X = 'Lux1X';
    cLux1Y = 'Lux1Y';
    cLux1Z = 'Lux1Z';
    nLux1 = 226;
    nLux1X = 227;
    nLux1Y = 228;
    nLux1Z = 229;

    cLux2  = 'Lux2';
    cLux2X = 'Lux2X';
    cLux2Y = 'Lux2Y';
    cLux2Z = 'Lux2Z';
    nLux2 = 230;
    nLux2X = 231;
    nLux2Y = 232;
    nLux2Z = 233;

    cLux3  = 'Lux3';
    cLux3X = 'Lux3X';
    cLux3Y = 'Lux3Y';
    cLux3Z = 'Lux3Z';
    nLux3 = 334;
    nLux3X = 335;
    nLux3Y = 336;
    nLux3Z = 337;

    cLux4 = 'Lux4';
    cLux4X = 'Lux4X';
    cLux4Y = 'Lux4Y';
    cLux4Z = 'Lux4Z';
    nLux4= 338;
    nLux4X = 339;
    nLux4Y = 340;
    nLux4Z = 341;

    cReducedMesh = 'ReducedMesh';
    nReducedMesh = 342;

    cPosZ1 = 'PosZ1';
    cPosZ2 = 'PosZ2';
    nPosZ1 = 343;
    nPosZ2 = 344;

    nDrawFigure = 345;

    cPin = 'Pin';
    cNorm = 'Norm';
    cTextureNorm = 'TextureNorm';
    nPin = 346;
    nNorm = 347;
    nTextureNorm = 348;

    cStripCount = 'StripCount';
    cBlindColorCount = 'BlindColorCount';
    cStripWidth = 'StripWidth';
    cBandWidth = 'BandWidth';
    cStandardColor = 'StandardColor';
    cStripColorMode = 'StripColorMode';
    cPixelMode = 'PixelMode';

    cWantContour = 'WantContour';
    cWantShirtColor = 'WantShirtColor';

    cSquareBitmap = 'SquareBitmap';
    cHorizontalBitmap = 'HorizontalBitmap';
    cSquareSym = 'SquareSym';

    cRandomPaint = 'RandomPaint';
    cStandardPaint = 'StandardPaint';
    cMonoPaint = 'MonoPaint';

    cStripColor = 'StripColor';
    cBackgroundColor = 'BackgroundColor';
    nStripCount = 349;
    nBlindColorCount = 350;
    nStripWidth = 350;
    nBandWidth = 351;
    nStandardColor = 352;
    nStripColorMode = 353;
    nPixelMode = 354;
    nWantContour = 355;
    nWantShirtColor = 356;
    nSquareBitmap = 357;
    nHorizontalBitmap = 358;
    nSquareSym = 359;
    nRandomPaint = 360;
    nStandardPaint = 361;
    nMonoPaint = 362;
    nBackgroundColor = 363;
    nStripColor = 364;


    cParamBandSelected = 'bs';
    nParamBandSelected = 365;

    cParamBandCount = 'bc';
    cParamBandDistributionX = 'bdx';
    cParamBandDistributionY = 'bdy';
    nParamBandCount = 366;
    nParamBandDistributionX = 367;
    nParamBandDistributionY = 368;

    //bw is one of (bwa, bwr, bwc)
    cParamBandWidthA ='bwa';
    cParamBandWidthR ='bwr';
    cParamBandWidthC ='bwc';
    cParamBandWidth = 'bw';
    nParamBandWidth = 369;

    cParamLabelTextX = 'ltx';
    cParamLabelTextY = 'lty';
    cParamLabelTextZ = 'ltz';
    nParamLabelTextX = 370;
    nParamLabelTextY = 371;
    nParamLabelTextZ = 372;

    cPan = 'pan';
    nPan = 373;

    cHullMesh = 'HullMesh';
    nHullMesh = 374;

    cInvertedMesh = 'InvertedMesh';
    nInvertedMesh = 375;

    cUprightMesh = 'UprightMesh';
    nUprightMesh = 376;

    cFlippedTexture = 'FlippedTexture';
    nFlippedTexture = 377;

    cx12 = 'x12';
    cy12 = 'y12';
    cz12 = 'z12';
    nx12 = 378;
    ny12 = 379;
    nz12 = 380;
    cParallelX12 = 'pX12';
    cParallelY12 = 'pY12';
    cParallelZ12 = 'pZ12';
    npx12 = 381;
    npy12 = 382;
    npz12 = 383;

    nStripColorOffset = 900;

    cla00 = 900;
    cla01 = 901;
    cla02 = 902;
    cla03 = 903;
    cla04 = 904;
    cla05 = 905;
    cla06 = 906;
    cla07 = 907;
    cla08 = 908;
    cla09 = 909;
    cla10 = 910;
    cla11 = 911;
    cla12 = 912;
    cla13 = 913;
    cla14 = 914;
    cla15 = 915;
    cla16 = 916;
    cla17 = 917;
    cla18 = 918;
    cla19 = 919;
    cla20 = 920;
    cla21 = 921;
    cla22 = 922;
    cla23 = 923;
    cla24 = 924;
    cla25 = 925;
    cla26 = 926;
    cla27 = 927;
    cla28 = 928;
    cla29 = 929;
    cla30 = 930;
    cla31 = 931;
    cla32 = 932;
    cla33 = 933;
    cla34 = 934;
//  end;

//---- von RiggVar.Rgg.Data

  cFaktor = 'Faktor';
  cName = 'Name';
//  cOffsetX = 'OffsetX'; //schon in FC
//  cOffsetZ = 'OffsetY'; //schon in FC

  cA0X = 'A0X';
  cA0Y = 'A0Y';
  cA0Z = 'A0Z';

  cC0X = 'C0X';
  cC0Y = 'C0Y';
  cC0Z = 'C0Z';

  cD0X = 'D0X';
  cD0Y = 'D0Y';
  cD0Z = 'D0Z';

  cE0X = 'E0X';
  cE0Y = 'E0Y';
  cE0Z = 'E0Z';

  cF0X = 'F0X';
  cF0Y = 'F0Y';
  cF0Z = 'F0Z';

  cMU = 'MU';
  cMO = 'MO';
  cML = 'ML';
  cMV = 'MV';
  cCA = 'CA';

  cCPMin = 'CPMin';
  cCPPos = 'CPPos';
  cCPMax = 'CPMax';

  cSHMin = 'SHMin';
  cSHPos = 'SHPos';
  cSHMax = 'SHMax';

  cSAMin = 'SAMin';
  cSAPos = 'SAPos';
  cSAMax = 'SAMax';

  cSLMin = 'SLMin';
  cSLPos = 'SLPos';
  cSLMax = 'SLMax';

  cSWMin = 'SWMin';
  cSWPos = 'SWPos';
  cSWMax = 'SWMax';

  cVOMin = 'VOMin';
  cVOPos = 'VOPos';
  cVOMax = 'VOMax';

  cWIMin = 'WIMin';
  cWIPos = 'WIPos';
  cWIMax = 'WIMax';

  cWLMin = 'WLMin';
  cWLPos = 'WLPos';
  cWLMax = 'WLMax';

  cWOMin = 'WOMin';
  cWOPos = 'WOPos';
  cWOMax = 'WOMax';

  cCP = 'cp';
  cSH = 'sh';
  cSA = 'sa';
//  cSL = 'sl';
//  cSW = 'sw';
  cVO = 'vo';
  cWI = 'wi';
  cWL = 'wl';
  cWO = 'wo';

// gespeicherte Basiswerte
  ch0 = 'h0';
//  cl2 = 'l2'; //schon in FC
  ch2 = 'h2';

  //nicht gespeichert
  ch1 = 'h1';
  ch3 = 'h3';
//  cl3 = 'l3'; //schon in FC
  cw3 = 'w3';


  cReset = 'Reset';
  cDownload = 'Download';
  cPlay = 'Play';
  cExecute = 'Execute';
  cPalCol = 'PalCol';

  //Copy of AlphaColors in System.UIConsts ( in implementation section)
  MyAlphaColors: array [0..147] of TIdentMapEntry = (
    (Value: Integer($FFF0F8FF); Name: 'claAliceblue'),
    (Value: Integer($FFFAEBD7); Name: 'claAntiquewhite'),
    (Value: Integer($FF00FFFF); Name: 'claAqua'),
    (Value: Integer($FF7FFFD4); Name: 'claAquamarine'),
    (Value: Integer($FFF0FFFF); Name: 'claAzure'),
    (Value: Integer($FFF5F5DC); Name: 'claBeige'),
    (Value: Integer($FFFFE4C4); Name: 'claBisque'),
    (Value: Integer($FF000000); Name: 'claBlack';),
    (Value: Integer($FFFFEBCD); Name: 'claBlanchedalmond'),
    (Value: Integer($FF0000FF); Name: 'claBlue'),
    (Value: Integer($FF8A2BE2); Name: 'claBlueviolet'),
    (Value: Integer($FFA52A2A); Name: 'claBrown'),
    (Value: Integer($FFDEB887); Name: 'claBurlywood'),
    (Value: Integer($FF5F9EA0); Name: 'claCadetblue'),
    (Value: Integer($FF7FFF00); Name: 'claChartreuse'),
    (Value: Integer($FFD2691E); Name: 'claChocolate'),
    (Value: Integer($FFFF7F50); Name: 'claCoral'),
    (Value: Integer($FF6495ED); Name: 'claCornflowerblue'),
    (Value: Integer($FFFFF8DC); Name: 'claCornsilk'),
    (Value: Integer($FFDC143C); Name: 'claCrimson'),
    (Value: Integer($FF00FFFF); Name: 'claCyan'),
    (Value: Integer($FF00008B); Name: 'claDarkblue'),
    (Value: Integer($FF008B8B); Name: 'claDarkcyan'),
    (Value: Integer($FFB8860B); Name: 'claDarkgoldenrod'),
    (Value: Integer($FFA9A9A9); Name: 'claDarkgray'),
    (Value: Integer($FF006400); Name: 'claDarkgreen'),
    (Value: Integer($FFA9A9A9); Name: 'claDarkgrey'),
    (Value: Integer($FFBDB76B); Name: 'claDarkkhaki'),
    (Value: Integer($FF8B008B); Name: 'claDarkmagenta'),
    (Value: Integer($FF556B2F); Name: 'claDarkolivegreen'),
    (Value: Integer($FFFF8C00); Name: 'claDarkorange'),
    (Value: Integer($FF9932CC); Name: 'claDarkorchid'),
    (Value: Integer($FF8B0000); Name: 'claDarkred'),
    (Value: Integer($FFE9967A); Name: 'claDarksalmon'),
    (Value: Integer($FF8FBC8F); Name: 'claDarkseagreen'),
    (Value: Integer($FF483D8B); Name: 'claDarkslateblue'),
    (Value: Integer($FF2F4F4F); Name: 'claDarkslategray'),
    (Value: Integer($FF2F4F4F); Name: 'claDarkslategrey'),
    (Value: Integer($FF00CED1); Name: 'claDarkturquoise'),
    (Value: Integer($FF9400D3); Name: 'claDarkviolet'),
    (Value: Integer($FFFF1493); Name: 'claDeeppink'),
    (Value: Integer($FF00BFFF); Name: 'claDeepskyblue'),
    (Value: Integer($FF696969); Name: 'claDimgray'),
    (Value: Integer($FF696969); Name: 'claDimgrey'),
    (Value: Integer($FF1E90FF); Name: 'claDodgerblue'),
    (Value: Integer($FFB22222); Name: 'claFirebrick'),
    (Value: Integer($FFFFFAF0); Name: 'claFloralwhite'),
    (Value: Integer($FF228B22); Name: 'claForestgreen'),
    (Value: Integer($FFFF00FF); Name: 'claFuchsia'),
    (Value: Integer($FFDCDCDC); Name: 'claGainsboro'),
    (Value: Integer($FFF8F8FF); Name: 'claGhostwhite'),
    (Value: Integer($FFFFD700); Name: 'claGold'),
    (Value: Integer($FFDAA520); Name: 'claGoldenrod'),
    (Value: Integer($FF808080); Name: 'claGray'),
    (Value: Integer($FF008000); Name: 'claGreen'),
    (Value: Integer($FFADFF2F); Name: 'claGreenyellow'),
    (Value: Integer($FF808080); Name: 'claGrey'),
    (Value: Integer($FFF0FFF0); Name: 'claHoneydew'),
    (Value: Integer($FFFF69B4); Name: 'claHotpink'),
    (Value: Integer($FFCD5C5C); Name: 'claIndianred'),
    (Value: Integer($FF4B0082); Name: 'claIndigo'),
    (Value: Integer($FFFFFFF0); Name: 'claIvory'),
    (Value: Integer($FFF0E68C); Name: 'claKhaki'),
    (Value: Integer($FFE6E6FA); Name: 'claLavender'),
    (Value: Integer($FFFFF0F5); Name: 'claLavenderblush'),
    (Value: Integer($FF7CFC00); Name: 'claLawngreen'),
    (Value: Integer($FFFFFACD); Name: 'claLemonchiffon'),
    (Value: Integer($FFADD8E6); Name: 'claLightblue'),
    (Value: Integer($FFF08080); Name: 'claLightcoral'),
    (Value: Integer($FFE0FFFF); Name: 'claLightcyan'),
    (Value: Integer($FFFAFAD2); Name: 'claLightgoldenrodyellow'),
    (Value: Integer($FFD3D3D3); Name: 'claLightgray'),
    (Value: Integer($FF90EE90); Name: 'claLightgreen'),
    (Value: Integer($FFD3D3D3); Name: 'claLightgrey'),
    (Value: Integer($FFFFB6C1); Name: 'claLightpink'),
    (Value: Integer($FFFFA07A); Name: 'claLightsalmon'),
    (Value: Integer($FF20B2AA); Name: 'claLightseagreen'),
    (Value: Integer($FF87CEFA); Name: 'claLightskyblue'),
    (Value: Integer($FF778899); Name: 'claLightslategray'),
    (Value: Integer($FF778899); Name: 'claLightslategrey'),
    (Value: Integer($FFB0C4DE); Name: 'claLightsteelblue'),
    (Value: Integer($FFFFFFE0); Name: 'claLightyellow'),
    (Value: Integer($FF00FF00); Name: 'claLime'),
    (Value: Integer($FF32CD32); Name: 'claLimegreen'),
    (Value: Integer($FFFAF0E6); Name: 'claLinen'),
    (Value: Integer($FFFF00FF); Name: 'claMagenta'),
    (Value: Integer($FF800000); Name: 'claMaroon'),
    (Value: Integer($FF66CDAA); Name: 'claMediumaquamarine'),
    (Value: Integer($FF0000CD); Name: 'claMediumblue'),
    (Value: Integer($FFBA55D3); Name: 'claMediumorchid'),
    (Value: Integer($FF9370DB); Name: 'claMediumpurple'),
    (Value: Integer($FF3CB371); Name: 'claMediumseagreen'),
    (Value: Integer($FF7B68EE); Name: 'claMediumslateblue'),
    (Value: Integer($FF00FA9A); Name: 'claMediumspringgreen'),
    (Value: Integer($FF48D1CC); Name: 'claMediumturquoise'),
    (Value: Integer($FFC71585); Name: 'claMediumvioletred'),
    (Value: Integer($FF191970); Name: 'claMidnightblue'),
    (Value: Integer($FFF5FFFA); Name: 'claMintcream'),
    (Value: Integer($FFFFE4E1); Name: 'claMistyrose'),
    (Value: Integer($FFFFE4B5); Name: 'claMoccasin'),
    (Value: Integer($FFFFDEAD); Name: 'claNavajowhite'),
    (Value: Integer($FF000080); Name: 'claNavy'),
    (Value: Integer($FFFDF5E6); Name: 'claOldlace'),
    (Value: Integer($FF808000); Name: 'claOlive'),
    (Value: Integer($FF6B8E23); Name: 'claOlivedrab'),
    (Value: Integer($FFFFA500); Name: 'claOrange'),
    (Value: Integer($FFFF4500); Name: 'claOrangered'),
    (Value: Integer($FFDA70D6); Name: 'claOrchid'),
    (Value: Integer($FFEEE8AA); Name: 'claPalegoldenrod'),
    (Value: Integer($FF98FB98); Name: 'claPalegreen'),
    (Value: Integer($FFAFEEEE); Name: 'claPaleturquoise'),
    (Value: Integer($FFDB7093); Name: 'claPalevioletred'),
    (Value: Integer($FFFFEFD5); Name: 'claPapayawhip'),
    (Value: Integer($FFFFDAB9); Name: 'claPeachpuff'),
    (Value: Integer($FFCD853F); Name: 'claPeru'),
    (Value: Integer($FFFFC0CB); Name: 'claPink'),
    (Value: Integer($FFDDA0DD); Name: 'claPlum'),
    (Value: Integer($FFB0E0E6); Name: 'claPowderblue'),
    (Value: Integer($FF800080); Name: 'claPurple'),
    (Value: Integer($FFFF0000); Name: 'claRed'),
    (Value: Integer($FFBC8F8F); Name: 'claRosybrown'),
    (Value: Integer($FF4169E1); Name: 'claRoyalblue'),
    (Value: Integer($FF8B4513); Name: 'claSaddlebrown'),
    (Value: Integer($FFFA8072); Name: 'claSalmon'),
    (Value: Integer($FFF4A460); Name: 'claSandybrown'),
    (Value: Integer($FF2E8B57); Name: 'claSeagreen'),
    (Value: Integer($FFFFF5EE); Name: 'claSeashell'),
    (Value: Integer($FFA0522D); Name: 'claSienna'),
    (Value: Integer($FFC0C0C0); Name: 'claSilver'),
    (Value: Integer($FF87CEEB); Name: 'claSkyblue'),
    (Value: Integer($FF6A5ACD); Name: 'claSlateblue'),
    (Value: Integer($FF708090); Name: 'claSlategray'),
    (Value: Integer($FF708090); Name: 'claSlategrey'),
    (Value: Integer($FFFFFAFA); Name: 'claSnow'),
    (Value: Integer($FF00FF7F); Name: 'claSpringgreen'),
    (Value: Integer($FF4682B4); Name: 'claSteelblue'),
    (Value: Integer($FFD2B48C); Name: 'claTan'),
    (Value: Integer($FF008080); Name: 'claTeal'),
    (Value: Integer($FFD8BFD8); Name: 'claThistle'),
    (Value: Integer($FFFF6347); Name: 'claTomato'),
    (Value: Integer($FF40E0D0); Name: 'claTurquoise'),
    (Value: Integer($FFEE82EE); Name: 'claViolet'),
    (Value: Integer($FFF5DEB3); Name: 'claWheat'),
    (Value: Integer($FFFFFFFF); Name: 'claWhite'),
    (Value: Integer($FFF5F5F5); Name: 'claWhitesmoke'),
    (Value: Integer($FFFFFF00); Name: 'claYellow'),
    (Value: Integer($FF9ACD32); Name: 'claYellowgreen'),
    (Value: Integer($0); Name: 'claNull')
  );

implementation

end.
