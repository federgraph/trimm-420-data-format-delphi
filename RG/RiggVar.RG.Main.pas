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
  RggStrings,
  RggUnit4,
  RiggVar.RG.Def,
  RiggVar.RG.Data,
  System.SysUtils,
  System.Classes,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Classes,
  RiggVar.Util.Logger;

type
  TRggMain = class
  private
    InitDataOK: Boolean;

    FParam: TFederParam;
    FAction: TFederAction;
    FTrimm: Integer;
    function GetShowTrimmText: Boolean;
    function GetShowDiffText: Boolean;
    function GetShowDataText: Boolean;
    procedure SetShowTrimmText(const Value: Boolean);
    procedure SetShowDiffText(const Value: Boolean);
    procedure SetShowDataText(const Value: Boolean);
    procedure DoReadTrimmFile(fn: string);
    function GetTrimmFilePath: string;
    function GetIsRggParam: Boolean;
    function LogFileNameToInfoFormatted(t1, t2, fn: string): boolean;
    procedure PasteTrimm;
    function GetColorScheme: Integer;
    procedure SetColorScheme(const Value: Integer);

    procedure SetParamValue(idx: TFederParam; const Value: single);
    function GetParamValue(idx: TFederParam): single;

    function GetCurrentTrimm: TRggData;
    procedure SetTrimm(const Value: Integer);
    procedure SetTrimmNoChange(const Value: Integer);
    procedure InitTrimmData;
    procedure SetParam(const Value: TFederParam);
  public
    IsUp: Boolean;
    Rigg: TRigg; // injected via constructor
    FL: TStringList;
    Logger: TLogger;

    RggData: TRggData; // temp object for data transfer

    { slot used as reference for diffing }
    Trimm0: TRggData;

    { user data slots }
    Trimm1: TRggData; // selected with button T1
    Trimm2: TRggData; // selected with button T2
    Trimm3: TRggData;
    Trimm4: TRggData;
    Trimm5: TRggData;
    Trimm6: TRggData;

    { example data slots }
    Trimm7: TRggData; // 420
    Trimm8: TRggData; // Logo

    BackgroundLock: Boolean;

    ReportCounter: Integer;
    ResizeCounter: Integer;

    constructor Create(ARigg: TRigg);
    destructor Destroy; override;

    function GetFLText: string;
    procedure CopyText;

    procedure LoadTrimm(fd: TRggData);
    procedure SaveTrimm(fd: TRggData);

    function GetTrimmItem(i: Integer): TRggData;
    function GetTrimmItemReport(ReportID: Integer): string;
    function GetTrimmItemReportData: string;
    function GetTrimmItemReportJson: string;
    function GetTrimmItemReportShort: string;
    function GetTrimmItemReportLong: string;

    procedure InitDefaultData;
    procedure Init420;
    procedure InitLogo;

    procedure UpdateTrimmText(ML: TStrings);

    function Param2Text(P: TFederParam): string;
    function Text2Param(T: string): TFederParam;

    procedure SetParameter(fa: TFederAction);

    procedure HandleAction(fa: Integer);
    function GetChecked(fa: TFederAction): Boolean;

    procedure DoTouchbarLeft(Delta: single);
    procedure DoTouchbarRight(Delta: single);
    procedure DoTouchbarBottom(Delta: single);
    procedure DoTouchbarTop(Delta: single);

    procedure ToggleDarkMode;
    procedure ToggleSpeedPanelFontSize;

    procedure UpdateOnParamValueChanged;

    procedure PlusOne;
    procedure PlusTen;
    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: Integer);
    procedure DoBigWheel(Delta: single);
    procedure DoSmallWheel(Delta: single);

    procedure WriteTrimmItem;
    procedure WriteTrimmFile;

    procedure CopyAndPaste;
    procedure CopyTrimmFile;
    procedure CopyTrimmItem;
    procedure PasteTrimmItem;

    procedure UpdateTrimm0;
    procedure ReadTrimmFile0;
    procedure ReadTrimmFile;
    procedure SaveTrimmFile;
    procedure ReadTrimmFileAuto;
    procedure SaveTrimmFileAuto;

    procedure ReadText(ML: TStrings);

    procedure DropTargetDropped(fn: string);
    procedure DoReport;
    procedure DoCleanReport;
    procedure ShowDebugData;
    procedure LogFileNameToInfo(fn: string);

    property FLText: string read GetFLText;
    property Param: TFederParam read FParam write SetParam;

    property CurrentTrimm: TRggData read GetCurrentTrimm;
    property TrimmNoChange: Integer read FTrimm write SetTrimmNoChange;
    property Trimm: Integer read FTrimm write SetTrimm;

    property TrimmData: string read GetTrimmItemReportData;
    property TrimmJson: string read GetTrimmItemReportJson;
    property TrimmShort: string read GetTrimmItemReportShort;
    property TrimmLong: string read GetTrimmItemReportLong;

    property ParamValue[index: TFederParam]: single read GetParamValue write SetParamValue;

    property ShowTrimmText: Boolean read GetShowTrimmText write SetShowTrimmText;
    property ShowDiffText: Boolean read GetShowDiffText write SetShowDiffText;
    property ShowDataText: Boolean read GetShowDataText write SetShowDataText;

    property IsRggParam: Boolean read GetIsRggParam;
    property ColorScheme: Integer read GetColorScheme write SetColorScheme;
  end;

implementation

uses
  System.Rtti,
  FMX.PlatForm,
  FrmMain,
  RiggVar.App.Main,
  RiggVar.Util.AppUtils;

{ TRggMain }

constructor TRggMain.Create(ARigg: TRigg);
begin
  inherited Create;
  Rigg := ARigg;

  Main := self;

  FParam := fpVorstag;

  FL := TStringList.Create;
  Logger := TLogger.Create;

  RggData := TRggData.Create;
  RggData.Name := 'fd';

  Trimm0 := TRggData.Create;
  Trimm0.Name := 'T0';
  Trimm7 := TRggData.Create;
  Trimm7.Name := '420';
  Trimm8 := TRggData.Create;
  Trimm8.Name := 'Logo';

  Trimm1 := TRggData.Create;
  Trimm2 := TRggData.Create;
  Trimm3 := TRggData.Create;
  Trimm4 := TRggData.Create;
  Trimm5 := TRggData.Create;
  Trimm6 := TRggData.Create;

  InitTrimmData;

  Rigg.Data.Reset;
end;

destructor TRggMain.Destroy;
begin
  MainVar.AppIsClosing := True;

  RggData.Free;
  Trimm0.Free;
  Trimm1.Free;
  Trimm2.Free;
  Trimm3.Free;
  Trimm4.Free;
  Trimm5.Free;
  Trimm6.Free;
  Trimm7.Free;
  Trimm8.Free;

  Rigg.Free;

  Logger.Free;
  FL.Free;

  inherited;
end;

procedure TRggMain.SetParameter(fa: TFederAction);
begin
  if FAction = faPan then
  begin
    FAction := faNoop;
    Exit;
  end;

  FAction := fa;
  case fa of
    faController: Param := fpController;
    faWinkel: Param := fpWinkel;
    faVorstag: Param := fpVorstag;
    faWante: Param := fpWante;
    faWoben: Param := fpWoben;
    faSalingH: Param := fpSalingH;
    faSalingA: Param := fpSalingA;
    faSalingL: Param := fpSalingL;
    faSalingW: Param := fpSalingW;
    faMastfallF0C: Param := fpMastfallF0C;
    faMastfallF0F: Param := fpMastfallF0F;
    faMastfallVorlauf: Param := fpMastfallVorlauf;
    faBiegung: Param := fpBiegung;
    faMastfussD0X: Param := fpD0X;
  end;
end;

function TRggMain.Text2Param(T: string): TFederParam;
begin
  result := fpVorstag;
  if T = ControllerString then
    result := fpController
  else if T = WinkelString then
    result := fpWinkel
  else if T = VorstagString then
    result := fpVorstag
  else if T = WanteString then
    result := fpWante
  else if (T = WanteObenString) or (T = 'Woben') then
    result := fpWoben
  else if (T = SalingHString) or (T = 'SalingH') then
    result := fpSalingH
  else if (T = SalingAString) or (T = 'SalingA') then
    result := fpSalingA
  else if (T = SalingLString) or (T = 'SalingL') then
    result := fpSalingL
  else if (T = SalingWString) or (T = 'SalingW') then
    result := fpSalingW
  else if T = MastfallF0CString then
    result := fpMastfallF0C
  else if T = MastfallF0FString then
    result := fpMastfallF0F
  else if T = MastfallVorlaufString then
    result := fpMastfallVorlauf
  else if T = BiegungString then
    result := fpBiegung
  else if T = MastFootD0XString then
    result := fpD0X
    ;
end;

function TRggMain.Param2Text(P: TFederParam): string;
begin
  result := '';
  if P = fpController then
    result := ControllerString
  else if P = fpWinkel then
    result := WinkelString
  else if P = fpVorstag then
    result := VorstagString
  else if P = fpWante then
    result := WanteString
  else if P = fpWoben then
    result := WanteObenString
  else if P = fpSalingH then
    result := SalingHString
  else if P = fpSalingA then
    result := SalingAString
  else if P = fpSalingL then
    result := SalingLString
  else if P = fpSalingW then
    result := SalingWString
  else if P = fpMastfallF0C then
    result := MastfallF0CString
  else if P = fpMastfallF0F then
    result := MastfallF0FString
  else if P = fpMastfallVorlauf then
    result := MastfallVorlaufString
  else if P = fpBiegung then
    result := BiegungString
  else if P = fpD0X then
    result := MastfootD0XString
    ;
end;

procedure TRggMain.SetTrimmNoChange(const Value: Integer);
begin
  Logger.Info('SetTrimmNoChange: ' + IntToStr(Value));
  FTrimm := Value;
end;

procedure TRggMain.SetTrimm(const Value: Integer);
begin
  Logger.Info('SetTrimm: ' + IntToStr(Value));
  FTrimm := Value;
  LoadTrimm(CurrentTrimm);
  UpdateOnParamValueChanged;
end;

function TRggMain.GetCurrentTrimm: TRggData;
begin
  result := GetTrimmItem(FTrimm);
end;

function TRggMain.GetTrimmItem(i: Integer): TRggData;
begin
  case i of
    1: result := Trimm1;
    2: result := Trimm2;
    3: result := Trimm3;
    4: result := Trimm4;
    5: result := Trimm5;
    6: result := Trimm6;
    7: result := Trimm7;
    8: result := Trimm8;
    else
      result := Trimm0;
  end;
end;

function TRggMain.GetTrimmItemReport(ReportID: Integer): string;
begin
  if (Rigg <> nil) and (RggData <> nil) and (FL <> nil) then
  begin
    Rigg.SaveToFederData(RggData);
    FL.Clear;
    case ReportID of
      0: RggData.WriteReport(FL);
      1: RggData.WriteJSon(FL);
      2:
      begin
        RggData.WantAll := False;
        RggData.SaveTrimmItem(FL);
      end;
      3:
      begin
        RggData.WantAll := True;
        RggData.SaveTrimmItem(FL);
      end;

      else
        RggData.WriteReport(FL);
    end;
    result := FL.Text;
    FL.Clear;
  end;
end;

function TRggMain.GetTrimmItemReportLong: string;
begin
  result := GetTrimmItemReport(3);
end;

function TRggMain.GetTrimmItemReportShort: string;
begin
  result := GetTrimmItemReport(2);
end;

function TRggMain.GetTrimmItemReportJson: string;
begin
  result := GetTrimmItemReport(1);
end;

function TRggMain.GetTrimmItemReportData: string;
begin
  result := GetTrimmItemReport(0);
end;

procedure TRggMain.InitTrimmData;
var
  fd: TRggData;
begin
  Logger.Info('in InitTrimmData ( default data )');
  fd := Trimm0;
  fd.Reset;

  fd := Trimm1;
  fd.Assign(Trimm0);
  fd.Name := 'T1';
  fd.MV := fd.MV + 10;

  fd := Trimm2;
  fd.Assign(Trimm0);
  fd.Name := 'T2';
  fd.WLPos := fd.WLPos + 20;

  fd := Trimm3;
  fd.Assign(Trimm0);
  fd.Name := 'T3';
  fd.VOPos := fd.VOPos + 30;

  fd := Trimm4;
  fd.Assign(Trimm0);
  fd.Name := 'T4';
  fd.SHPos := fd.SHPos + 40;

  fd := Trimm5;
  fd.Assign(Trimm0);
  fd.Name := 'T5';
  fd.SAPos := fd.SAPos + 50;

  fd := Trimm6;
  fd.Assign(Trimm0);
  fd.Name := 'T6';
  fd.VOPos := fd.VOPos + 60;
  fd.WLPos := fd.WLPos - 20;
end;

function TRggMain.GetFLText: string;
begin
  result := FL.Text;
end;

procedure TRggMain.CopyText;
var
  cbs: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(cbs)) then
  begin
    cbs.SetClipboard(FL.Text);
    Logger.Info('in CopyText ( check clipboard )');
  end;
end;

procedure TRggMain.SetColorScheme(const Value: Integer);
begin
  if not BackgroundLock then
  begin
    MainVar.ColorScheme.Scheme := Value;
    MainVar.ColorScheme.Init(Value);
  end;

  if IsUp then
  begin
//    FederText.UpdateColorScheme;
    FormMain.UpdateColorScheme;
  end;
end;

procedure TRggMain.ToggleSpeedPanelFontSize;
begin
  FormMain.ToggleSpeedPanelFontSize;
end;

procedure TRggMain.ToggleDarkMode;
begin
  if MainVar.ColorScheme.IsDark then
    ColorScheme := MainVar.ColorScheme.Light
  else
    ColorScheme := MainVar.ColorScheme.Dark;
end;

function TRggMain.GetColorScheme: Integer;
begin
  result := MainVar.ColorScheme.Scheme;
end;

procedure TRggMain.DoTouchbarLeft(Delta: single);
begin
//  DoMouseWheel([ssShift], Round(Delta));
end;

procedure TRggMain.DoTouchbarTop(Delta: single);
begin
//  FormMain.RotaForm.RotateZ(Delta);
end;

procedure TRggMain.DoTouchbarRight(Delta: single);
begin
  DoMouseWheel([], Round(Delta));
end;

procedure TRggMain.DoTouchbarBottom(Delta: single);
begin
//  FormMain.RotaForm.Zoom(Delta);
end;

procedure TRggMain.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer);
var
  wd: Integer;
begin
  wd := WheelDelta; // small values may come from touchpad

  { normal mouse }
  if Abs(WheelDelta) > 100 then
    wd := WheelDelta div 120;;

  if ssCtrl in Shift then
  begin
//    FormMain.RotaForm.Zoom(wd);
  end
  else if ssShift in Shift then
  begin
    DoBigWheel(wd);
  end
  else if Shift = [] then
  begin
    DoSmallWheel(wd);
  end;
end;

procedure TRggMain.PlusOne;
begin
  DoMouseWheel([ssShift], 1);
end;

procedure TRggMain.PlusTen;
begin
  DoMouseWheel([ssCtrl], 1);
end;

procedure TRggMain.UpdateOnParamValueChanged;
begin
  FormMain.UpdateOnParamValueChanged;
end;

procedure TRggMain.SetShowTrimmText(const Value: Boolean);
begin
  FormMain.ShowTrimmText := Value;
end;

procedure TRggMain.SetShowDiffText(const Value: Boolean);
begin
  FormMain.ShowDiffText := Value;
end;

procedure TRggMain.SetShowDataText(const Value: Boolean);
begin
  FormMain.ShowDataText := Value;
end;

function TRggMain.GetShowTrimmText: Boolean;
begin
  result := FormMain.ShowTrimmText;
end;

function TRggMain.GetShowDiffText: Boolean;
begin
  result := FormMain.ShowDiffText;
end;

function TRggMain.GetShowDataText: Boolean;
begin
  result := FormMain.ShowDataText;
end;

procedure TRggMain.WriteTrimmItem;
var
  fd: TRggData;
begin
  FL.Clear;
  fd := RggData;
  SaveTrimm(fd);
  fd.WantAll := True;
  fd.SaveTrimmItem(FL);
end;

procedure TRggMain.CopyTrimmItem;
begin
  Logger.Info('in CopyTrimmItem');
  WriteTrimmItem;
  CopyText;
  FL.Clear;
  FormMain.ShowTrimm;
end;

procedure TRggMain.WriteTrimmFile;
begin
  FL.Clear;
  Trimm0.SaveTrimmFile(FL);
end;

procedure TRggMain.CopyTrimmFile;
begin
  WriteTrimmFile;
  CopyText;
  FL.Clear;
  FormMain.ShowTrimm;
end;

procedure TRggMain.CopyAndPaste;
var
  fd: TRggData;
begin
  { copy }
  fd := RggData;
  SaveTrimm(fd);
  fd.WantAll := True;
  fd.SaveTrimmItem(FL);

  { paste }
  ReadText(FL);
  FL.Clear;
  FormMain.ShowTrimm;
end;

procedure TRggMain.PasteTrimmItem;
begin
  Logger.Info('in PasteTrimmItem');
  PasteTrimm;
  { Note: There is just one paste button (pti), named after the item, }
  { but you can paste a Trimm-Item OR a Trimm-File. }
  FormMain.ShowTrimm;
end;

procedure TRggMain.PasteTrimm;
var
  v: TValue;
  s: string;
  cbs: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(cbs)) then
  begin
    FL.Clear;
    try
      v := cbs.GetClipboard;
      if not v.IsEmpty then
      begin
        s := v.AsString;
        if (s <> '') and (Length(s) < 10000) then
        begin
          FL.Text := s;
          ReadText(FL);
          FL.Clear;
        end
        else
        begin
          Logger.Error('there is no ''data'' string on the clipboard');
        end;
      end;
    except
      Logger.Error('no usable data on clipboard');
    end;
  end;
end;

procedure TRggMain.ReadText(ML: TStrings);
var
  i: Integer;
  s: string;
  IsTrimmItem: Boolean;
  IsTrimmFile: Boolean;
begin
  try
    IsTrimmItem := False;
    IsTrimmFile := False;
    for i := 0 to ML.Count-1 do
    begin
      if i > 4 then
        break;
      s := ML[i];
      if Pos('DOCTYPE', s) > 0 then
      begin
        if s.Contains('Trimm-Item' )then
          IsTrimmItem := True;
        if s.Contains('Trimm-File' )then
          IsTrimmFile := True;
      end;
      if IsTrimmItem then
      begin
        RggData.LoadTrimmItem(ML);
        CurrentTrimm.Assign(RggData);
        Trimm := FTrimm;
        Logger.Info(Format('  Trimm %d assigned', [Trimm]));
        break;
      end
      else if IsTrimmFile then
      begin
        RggData.ReadTrimmFile(ML);
        Trimm := FTrimm;
        Logger.Info(Format('  TrimmFile read, Current Trimm is %d.', [Trimm]));
        break;
      end;
    end;
  except
    Trimm := 1;
  end;
end;

function TRggMain.GetTrimmFilePath: string;
begin
  result := '';
{$ifdef MacOS}
  result  := TAppUtils.GetDocumentDirectory;
{$endif}
{$ifdef Win32}
  result  := TAppUtils.GetUserDocumentsDir;
  //result  := TAppUtils.GetRoamingDir;
{$endif}
{$ifdef Win64}
  result  := TAppUtils.GetUserDocumentsDir;
  //result  := TAppUtils.GetRoamingDir;
{$endif}
{$ifdef Android}
  result  := TAppUtils.GetDocumentDirectory;
{$endif}
{$ifdef IOS}
  result  := TAppUtils.GetIOSDataDirectory;
{$endif}
end;

procedure TRggMain.ReadTrimmFile0;
var
  fp, sTrimmFileAuto, sTrimmFile: string;
begin
  if not MainVar.IsSandBoxed then
  begin
    Logger.Info('in ReadTrimmFile0');
    fp := GetTrimmFilePath;
    if fp <> '' then
    begin
      sTrimmFile := fp + MainConst.TrimmFileName;
      sTrimmFileAuto := fp + MainConst.TrimmFileNameAuto;
      if FileExists(sTrimmFile) then
      begin
        DoReadTrimmFile(sTrimmFile);
      end
      else if FileExists(sTrimmFileAuto) then
      begin
        DoReadTrimmFile(sTrimmFileAuto);
      end
      else
      begin
        Logger.Info('Trimm-File.txt or Trimm-File-Auto.txt does not exits at path');
        LogFileNameToInfo(fp);
      end;
    end
    else
    begin
      Logger.Info('GetTrimmFilePath is empty');
    end;
  end;
end;

procedure TRggMain.ReadTrimmFile;
var
  fp, fn, s: string;
begin
  Logger.Info('in ReadTrimmFile');
  fp := GetTrimmFilePath;

{ By default you try and load the 'manually edited' Trimm-File.txt; }
{ this should make sense on the Desktop, }
{ or on any device where you have access to the Documents folder. }
  fn := MainConst.TrimmFileName;

{ Maybe you want to have the same behaviour on Windows and iOS }
{ for debugging purpose only... }
{$ifdef MSWINDOWS}
//  fn := TrimmFileNameAuto;
{$endif}

{ On Android and iOS the Trimm-File in the known location cannot be edited, }
{ so it does not make sense to read a 'manually edited' Trimm-File.txt, }
{ but you can manually read a Trimm-File-Auto.txt if already saved, }
{ e.g. by clicking on a button. }
{$ifdef IOS}
  fn := MainConst.TrimmFileNameAuto;
{$endif}
{$ifdef Android}
  fn := MainConst.TrimmFileNameAuto;
{$endif}

  s := fp + fn;
  if MainVar.IsSandboxed then
  begin
    s := FormMain.GetOpenFileName(fp, fn);
  end;

  if s <> '' then
  begin
    DoReadTrimmFile(s);
  end;

  FormMain.ShowTrimm;
end;

procedure TRggMain.ReadTrimmFileAuto;
var
  fp, fn: string;
begin
  if not MainVar.IsSandboxed then
  begin
    Logger.Info('in ReadTrimmFileAuto');
    fp := GetTrimmFilePath;
    fn := MainConst.TrimmFileNameAuto;
    if (fp <> '') and (fn <> '') then
      DoReadTrimmFile(fp + fn);
  end;
end;

procedure TRggMain.DoReadTrimmFile(fn: string);
begin
  Logger.Info('in DoReadTrimmFile');
  if (fn <> '') and FileExists(fn) then
  begin
    try
      try
        LogFileNameToInfo(fn);
        FL.LoadFromFile(fn, TEncoding.UTF8);
        RggData.ReadTrimmFile(FL);
        Trimm := FTrimm;
      except
        Logger.Info('  in exeption handler');
        InitTrimmData;
      end;
    finally
      FL.Clear;
    end;
  end
  else
  begin
    Logger.Info('TrimmFile does not exist');
    LogFileNameToInfo(fn);
    Logger.Info('Nothing read.');
    InitTrimmData;
  end;
end;

procedure TRggMain.SaveTrimmFile;
begin
  Logger.Info('in SaveTrimmFile');
  SaveTrimmFileAuto;
  FormMain.ShowTrimm;
end;

procedure TRggMain.SaveTrimmFileAuto;
var
  fp, fn, s: string;
begin
  Logger.Info('in SaveTrimmFileAuto');
  fp := GetTrimmFilePath;
  fn := MainConst.TrimmFileNameAuto;

  s := fp + fn;
  if MainVar.IsSandboxed then
  begin
    s := FormMain.GetSaveFileName(fp, fn);
  end;

  if s <> '' then
  begin
    Trimm0.SaveTrimmFile(FL);
    LogFileNameToInfo(s);
    FL.SaveToFile(s);
    Logger.Info('TrimmFileAuto saved.');
  end
  else
  begin
    Logger.Info('Nothing saved.');
  end;
end;

procedure TRggMain.UpdateTrimm0;
begin
  Logger.Info('in UpdateTrimm0');
  SaveTrimm(Trimm0);
  FormMain.UpdateReport;
  FormMain.ShowTrimm;
end;

function TRggMain.GetIsRggParam: Boolean;
begin
  result := True;
end;

procedure TRggMain.DropTargetDropped(fn: string);
var
 ext: string;
begin
  Logger.Info('in DropTargetDropped');
  ext := ExtractFileExt(fn);
  if ext = '.txt' then
  begin
    DoReadTrimmFile(fn);
  end
  else
  begin
    Logger.Info('  .txt file expected');
  end;
end;

procedure TRggMain.DoReport;
var
  ML: TStrings;
begin
  Inc(ReportCounter);
  ML := Logger.TL;

  while ML.Count > 24 - 8 do
    ML.Delete(0);

  ML.Add('Report:');
  ML.Add('  ReportCounter = ' + IntToStr(ReportCounter));
  ML.Add('  Sandboxed = ' + BoolStr[MainVar.IsSandboxed]);
  ML.Add('  WantOnResize = ' + BoolStr[MainVar.WantOnResize]);
  ML.Add('  ResizeCounter = ' + IntToStr(ResizeCounter));
  ML.Add(Format('  ClientSize = (%d, %d)', [MainVar.ClientWidth, MainVar.ClientHeight]));
  ML.Add('---');
end;

procedure TRggMain.DoCleanReport;
begin
  MainVar.ShowDebugData := True;
  Logger.TL.Clear;
  DoReport;
end;

procedure TRggMain.ShowDebugData;
begin
  if not ShowDataText then
  begin
    ShowDataText := true;
    MainVar.ShowDebugData := True;
  end
  else
  begin
    MainVar.ShowDebugData := not MainVar.ShowDebugData;
  end;
end;

procedure TRggMain.LogFileNameToInfo(fn: string);
var
  t1: string;
  t2: string;
  r: boolean;
begin
  t1 := 'C:\Users\';
  t2 := 'Trimm';
  r := LogFileNameToInfoFormatted(t1, t2, fn);

  if not r then
  begin
    t1 := '/var/mobile/Containers/Data/Application/';
    t2 := 'Documents/';
    r := LogFileNameToInfoFormatted(t1, t2, fn);
  end;

  if not r then
  begin
    Logger.Info(fn);
  end;
end;

function TRggMain.LogFileNameToInfoFormatted(t1: string; t2: string; fn: string): boolean;
var
  p2: Integer;
begin
  result := false;
  if fn.StartsWith(t1) then
  begin
    fn := fn.Substring(t1.Length);
    p2 := fn.LastIndexOf(t2);
    if p2 > -1 then
    begin
      Logger.Info(t1);
      Logger.Info('  ' + fn.Substring(0, p2));
      Logger.Info('  ' + fn.Substring(p2));
      result := True;
    end;
  end;
end;

procedure TRggMain.HandleAction(fa: Integer);
begin
  if IsUp then
  case fa of
    faUpdateReportText: DoCleanReport;
    faToggleDebugText: ShowDebugData;

    faParamValueMinus1, faWheelLeft: DoMouseWheel([], -1);
    faParamValuePlus1, faWheelRight: DoMouseWheel([], 1);
    faParamValuePlus10, faWheelUp: DoMouseWheel([ssShift], 1);
    faParamValueMinus10, faWheelDown: DoMouseWheel([ssShift], -1);

    faTrimm0: Trimm := 0;
    faTrimm1: Trimm := 1;
    faTrimm2: Trimm := 2;
    faTrimm3: Trimm := 3;
    faTrimm4: Trimm := 4;
    faTrimm5: Trimm := 5;
    faTrimm6: Trimm := 6;

    fa420: Init420;
    faLogo: InitLogo;

    faUpdateTrimm0: UpdateTrimm0;
    faCopyAndPaste: CopyAndPaste;
    faCopyTrimmItem: CopyTrimmItem;
    faPasteTrimmItem: PasteTrimmItem;

    faReadTrimmFile: ReadTrimmFile;
    faCopyTrimmFile: CopyTrimmFile;
    faSaveTrimmFile: SaveTrimmFile;

    faToggleTrimmText: ShowTrimmText := not ShowTrimmText;
    faToggleDiffText: ShowDiffText := not ShowDiffText;
    faToggleDataText:
    begin
      MainVar.ShowDebugData := False;
      ShowDataText := not ShowDataText;
    end;

    else
    begin
      FormMain.HandleAction(fa);
    end;
  end;

end;

function TRggMain.GetChecked(fa: TFederAction): Boolean;
//var
//  F: TFormMain;
begin
//  F := FormMain;
  result := false;

(*
  if not IsUp then
    Exit;

  case fa of
    faController: result := Param = fpController;
    faWinkel: result := Param = fpWinkel;
    faVorstag: result := Param = fpVorstag;
    faWante: result := Param = fpWante;
    faWoben: result := Param = fpWoben;
    faSalingH: result := Param = fpSalingH;
    faSalingA: result := Param = fpSalingA;
    faSalingL: result := Param = fpSalingL;
    faSalingW: result := Param = fpSalingW;
    faMastfallF0C: result := Param = fpMastfallF0C;
    faMastfallF0F: result := Param = fpMastfallF0F;
    faMastfallVorlauf: result := Param = fpMastfallVorlauf;
    faBiegung: result := Param = fpBiegung;
    faMastfussD0X: result := Param = fpD0X;

    faParamAPW: result := Param = fpAPW;
    faParamEAH: result := Param = fpEAH;
    faParamEAR: result := Param = fpEAR;
    faParamEI: result := Param = fpEI;

    faPan: result := Action = faPan;

    faFixpointA0: result := FixPoint = ooA0;
    faFixpointA: result := FixPoint = ooA;
    faFixpointB0: result := FixPoint = ooB0;
    faFixpointB: result := FixPoint = ooB;
    faFixpointC0: result := FixPoint = ooC0;
    faFixpointC: result := FixPoint = ooC;
    faFixpointD0: result := FixPoint = ooD0;
    faFixpointD: result := FixPoint = ooD;
    faFixpointE0: result := FixPoint = ooE0;
    faFixpointE: result := FixPoint = ooE;
    faFixpointF0: result := FixPoint = ooF0;
    faFixpointF: result := FixPoint = ooF;

    faSalingTypFest: result := Rigg.SalingTyp = stFest;
    faSalingTypDrehbar: result := Rigg.SalingTyp = stDrehbar;
    faSalingTypOhne: result := Rigg.SalingTyp = stOhneBiegt;
    faSalingTypOhneStarr: result := Rigg.SalingTyp = stOhneStarr;

    faTrimm0: result := Trimm = 0;
    faTrimm1: result := Trimm = 1;
    faTrimm2: result := Trimm = 2;
    faTrimm3: result := Trimm = 3;
    faTrimm4: result := Trimm = 4;
    faTrimm5: result := Trimm = 5;
    faTrimm6: result := Trimm = 6;
    fa420: result := Trimm = 7;
    faLogo: result := Trimm = 8;

    faRggBogen,
    faRggKoppel,
    faWantRenderH,
    faWantRenderP,
    faWantRenderF,
    faWantRenderE,
    faWantRenderS: result := StrokeRigg.QueryRenderOption(fa);

    faRggHull: result := HullVisible;
    faDemo: result := Demo;

    faSofortBtn: result := SofortBerechnen;
    faGrauBtn: result := BtnGrauDown;
    faBlauBtn: result := BtnBlauDown;
    faMemoryBtn: result := False;

    faSuperSimple: result := GraphRadio = gSimple;
    faSuperNormal: result := GraphRadio = gNormal;
    faSuperGrau: result := GraphRadio = gGrau;
    faSuperBlau: result := GraphRadio = gBlau;
    faSuperMulti: result := GraphRadio = gMulti;
    faSuperDisplay: result := GraphRadio = gDisplay;
    faSuperQuick: result := GraphRadio = gQuick;

    faToggleHelp: result := F.HelpText.Visible;
    faToggleReport: result := F.ReportText.Visible;
    faToggleButtonReport: result := F.WantButtonReport;
    faReportNone..faReportReadme: result := F.ReportManager.GetChecked(fa);

    faToggleDataText: result := F.ShowDataText;
    faToggleDiffText: result := F.ShowDiffText;
    faToggleTrimmText: result := F.ShowTrimmText;

    faToggleFontColor: result := MainVar.ColorScheme.IsDark;

    else
      result := F.GetChecked(fa);
  end;
*)
end;

procedure TRggMain.InitDefaultData;
begin
  if not InitDataOK then
  begin
    InitDataOK := True;
    InitLogo; // sets WantLogoData to true
    Init420; // resets WantLogoData to false
    Trimm := 1;
//    FixPoint := InitialFixPoint;
  end;
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
  SaveTrimm(Trimm7);
  TrimmNoChange := 7;

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

  SaveTrimm(Trimm8);
  TrimmNoChange := 8;

  Rigg.Data.Name := 'Rigg.Data';
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

procedure TRggMain.DoSmallWheel(Delta: single);
begin
  if Delta > 0 then
    ParamValue[fpVorstag] := ParamValue[fpVorstag] + 1
  else
    ParamValue[fpVorstag] := ParamValue[fpVorstag] - 1;
end;

procedure TRggMain.DoBigWheel(Delta: single);
begin
  if Delta > 0 then
    ParamValue[fpVorstag] := ParamValue[fpVorstag] + 5
  else
    ParamValue[fpVorstag] := ParamValue[fpVorstag] - 5;
end;

procedure TRggMain.LoadTrimm(fd: TRggData);
begin
  Rigg.LoadFromFederData(fd);
end;

procedure TRggMain.SaveTrimm(fd: TRggData);
begin
  Rigg.SaveToFederData(fd);
end;

function TRggMain.GetParamValue(idx: TFederParam): single;
begin
  if idx = fpVorstag then
    result := Rigg.Data.VOPos
  else
    result := 0;
end;

procedure TRggMain.SetParamValue(idx: TFederParam; const Value: single);
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

end.
