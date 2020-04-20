unit RiggVar.RG.Main;

(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)

interface

uses
  RggUnit4,
  RiggVar.RG.Def,
  RiggVar.RG.Data,
  System.SysUtils,
  System.Classes;

type
  TRggMain = class
  private
    FParam: TFederParam;
    procedure SetParam(const Value: TFederParam);
    function GetParamValue(index: TFederParam): single;
    procedure SetParamValue(index: TFederParam; const Value: single);
  public
    Rigg: TRigg; // owned, passed in via constructor

    constructor Create(ARigg: TRigg);
    destructor Destroy; override;

    procedure Init;

    procedure UpdateTrimmText(ML: TStrings);

    procedure DoBigWheel(Delta: single);
    procedure DoSmallWheel(Delta: single);

    procedure LoadTrimm(fd: TRggData);
    procedure SaveTrimm(fd: TRggData);

    procedure Init420;
    procedure InitLogo;

    function Param2Text(P: TFederParam): string;
    function Text2Param(T: string): TFederParam;

    property Param: TFederParam read FParam write SetParam;
    property ParamValue[index: TFederParam]: single read GetParamValue write SetParamValue;
  end;

implementation

uses
  RiggVar.App.Main;

{ TRggMain }

constructor TRggMain.Create(ARigg: TRigg);
begin
  inherited Create;
  Rigg := ARigg;
  FParam := fpVorstag;
  Init;
end;

destructor TRggMain.Destroy;
begin
  Rigg.Free;
  inherited;
end;

procedure TRggMain.DoBigWheel(Delta: single);
begin
  if Delta > 0 then
    ParamValue[fpVorstag] := ParamValue[fpVorstag] + 5
  else
    ParamValue[fpVorstag] := ParamValue[fpVorstag] - 5;
end;

procedure TRggMain.DoSmallWheel(Delta: single);
begin
  if Delta > 0 then
    ParamValue[fpVorstag] := ParamValue[fpVorstag] + 1
  else
    ParamValue[fpVorstag] := ParamValue[fpVorstag] - 1;
end;

function TRggMain.GetParamValue(index: TFederParam): single;
begin
  if index = fpVorstag then
    result := Rigg.Data.VOPos
  else
    result := 0;
end;

procedure TRggMain.SetParamValue(index: TFederParam; const Value: single);
var
  t: Integer;
begin
  if FParam = fpVorstag then
  begin
    t := Round(Value);
    if t < Rigg.Data.VOMin then
      t := rigg.Data.VOMin;
    if t > Rigg.Data.VOMax then
      t := rigg.Data.VOMax;
    Rigg.Data.VOPos := t;
  end;
end;

procedure TRggMain.Init;
begin
  Rigg.Data.Reset;
end;

procedure TRggMain.Init420;
begin
  with Rigg.Data do
  begin
    Name := '420';
    A0X := 2560;
    A0Y := 765;
    A0Z := 430;
    C0X := 4140;
    C0Y := 0;
    C0Z := 340;
    D0X := 2870;
    D0Y := 0;
    D0Z := -100;
    E0X := 2970;
    E0Y := 0;
    E0Z := 450;
    F0X := -30;
    F0Y := 0;
    F0Z := 300;
    MU := 2600;
    MO := 2000;
    ML := 6115;
    MV := 5000;
    CA := 50;
    h0 := 56;
    h2 := 0;
    l2 := 100;
    CPMin := 0;
    CPPos := 100;
    CPMax := 200;
    SHMin := 170;
    SHPos := 220;
    SHMax := 1020;
    SAMin := 250;
    SAPos := 850;
    SAMax := 1550;
    SLMin := 240;
    SLPos := 479;
    SLMax := 1200;
    SWMin := 15;
    SWPos := 27;
    SWMax := 87;
    VOMin := 4200;
    VOPos := 4500;
    VOMax := 5000;
    WIMin := 80;
    WIPos := 94;
    WIMax := 115;
    WLMin := 4020;
    WLPos := 4120;
    WLMax := 4220;
    WOMin := 2000;
    WOPos := 2020;
    WOMax := 2100;
  end;
  SaveTrimm(Main.Trimm7);
  Main.TrimmNoChange := 7;

  Rigg.Data.Name := 'Rigg.Data';
end;

procedure TRggMain.InitLogo;
begin
  with Rigg.Data do
  begin
    Name := 'Logo';
    A0X := 1940;
    A0Y := 720;
    A0Z := 370;
    C0X := 4100;
    C0Y := 0;
    C0Z := 370;
    D0X := 2840;
    D0Y := 0;
    D0Z := -170;
    E0X := 2930;
    E0Y := 0;
    E0Z := 550;
    F0X := -130;
    F0Y := 0;
    F0Z := 370;
    MU := 1708;
    MO := 1138;
    ML := 3566;
    MV := 2674;
    CA := 50;
    h0 := 56;
    h2 := 0;
    l2 := 100;
    CPMin := 0;
    CPPos := 100;
    CPMax := 200;
    SHMin := 620;
    SHPos := 720;
    SHMax := 820;
    SAMin := 1340;
    SAPos := 1440;
    SAMax := 1540;
    SLMin := 918;
    SLPos := 1018;
    SLMax := 1118;
    SWMin := 40;
    SWPos := 45;
    SWMax := 60;
    VOMin := 2955;
    VOPos := 3055;
    VOMax := 3155;
    WIMin := 70;
    WIPos := 108;
    WIMax := 120;
    WLMin := 2385;
    WLPos := 2485;
    WLMax := 2585;
    WOMin := 1247;
    WOPos := 1347;
    WOMax := 1447;
  end;

  SaveTrimm(Main.Trimm8);
  Main.TrimmNoChange := 8;

  Rigg.Data.Name := 'Rigg.Data';
end;

procedure TRggMain.LoadTrimm(fd: TRggData);
begin
  Rigg.LoadFromFederData(fd);
end;

procedure TRggMain.SaveTrimm(fd: TRggData);
begin
  Rigg.SaveToFederData(fd);
end;

procedure TRggMain.SetParam(const Value: TFederParam);
begin
  FParam := Value;
end;

procedure TRggMain.UpdateTrimmText(ML: TStrings);
begin
  ML.Clear;
  ML.Add('V0Pos = ' + IntToStr(Rigg.Data.VOPos));
end;

function TRggMain.Text2Param(T: string): TFederParam;
begin
  result := fpT1;
  if T = 'Controller' then
    result := fpController
  else if T = 'Winkel' then
    result := fpWinkel
  else if T = 'Vorstag' then
    result := fpVorstag
  else if T = 'Wante' then
    result := fpWante
  else if (T = 'Wante oben') or (T = 'Woben') then
    result := fpWoben
  else if (T = 'Saling Höhe') or (T = 'SalingH') then
    result := fpSalingH
  else if (T = 'Saling Abstand') or (T = 'SalingA') then
    result := fpSalingA
  else if (T = 'Saling Länge') or (T = 'SalingL') then
    result := fpSalingL
  else if (T = 'Saling Winkel') or (T = 'SalingW') then
    result := fpSalingW
  else if T = 'Mastfall F0C' then
    result := fpMastfallF0C
  else if T = 'Mastfall F0F' then
    result := fpMastfallF0F
  else if T = 'Mastfall Vorlauf' then
    result := fpMastfallVorlauf
  else if T = 'Biegung' then
    result := fpBiegung
  else if T = 'Mastfuß D0x' then
    result := fpD0X
  else if T = 't1' then
    result := fpT1
  else if T = 't2' then
    result := fpT2
    ;
end;

function TRggMain.Param2Text(P: TFederParam): string;
begin
  result := '';
  if P = fpController then
    result := 'Controller'
  else if P = fpWinkel then
    result := 'Winkel'
  else if P = fpVorstag then
    result := 'Vorstag'
  else if P = fpWante then
    result := 'Wante'
  else if P = fpWoben then
    result := 'Wante oben'
  else if P = fpSalingH then
    result := 'Saling Höhe'
  else if P = fpSalingA then
    result := 'Saling Abstand'
  else if P = fpSalingL then
    result := 'Saling Länge'
  else if P = fpSalingW then
    result := 'Saling Winkel'
  else if P = fpMastfallF0C then
    result := 'Mastfall F0C'
  else if P = fpMastfallF0F then
    result := 'Mastfall F0F'
  else if P = fpMastfallVorlauf then
    result := 'Mastfall Vorlauf'
  else if P = fpBiegung then
    result := 'Biegung'
  else if P = fpD0X then
    result := 'Mastfuß D0x'
  else if P = fpT1 then
    result := 't1'
  else if P = fpT2 then
    result := 't2'
    ;
end;

end.
