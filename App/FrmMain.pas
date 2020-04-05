unit FrmMain;

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
  RiggVar.RG.Def,
  RiggVar.RG.Report,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.UIConsts,
  FMX.Platform,
  FMX.Graphics,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Listbox,
  FMX.Dialogs,
  FMX.Styles.Objects,
  FMX.Controls.Presentation;

{$define FMX}

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MemoMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  private
    procedure UpdateLog;
    procedure UpdateMemo;
    procedure UpdateReport;
    procedure UpdateJson;
  private
    TL: TStrings;
    RL: TStrings;
    CurrentReport: TRggReport;
    procedure InitTrimmCombo;
    procedure InitParamCombo;
    procedure InitReportListbox;
    procedure ACI(fp: TFederParam);
    procedure ShowTrimm;
    procedure ShowCurrentReport;
    function GetReportCaption(r: TRggReport): string;
    procedure HandleShowHint(Sender: TObject);
  public
    procedure HandleAction(fa: Integer);
  public
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
  public
    T1Btn: TSpeedButton;
    T2Btn: TSpeedButton;
    T3Btn: TSpeedButton;

    LogBtn: TSpeedButton;
    ReportBtn: TSpeedButton;
    StateBtn: TSpeedButton;
    JsonBtn: TSpeedButton;
  public
    HintText: TText;
    SpeedPanel: TPanel;
    TrimmMemo: TMemo;
    ReportListbox: TListBox;
    ReportLabel: TText;
    ReportMemo: TMemo;
    LogMemo: TMemo;

    TrimmCombo: TComboBox;
    ParamCombo: TComboBox;

    MT0Btn: TSpeedButton;
    ReadTrimmFileBtn: TSpeedButton;
    SaveTrimmFileBtn: TSpeedButton;

    CopyTrimmItemBtn: TSpeedButton;
    PasteTrimmItemBtn: TSpeedButton;
    CopyAndPasteBtn: TSpeedButton;

    M1Btn: TSpeedButton;
    M10Btn: TSpeedButton;
    P1Btn: TSpeedButton;
    P10Btn: TSpeedButton;

    SandboxedBtn: TSpeedButton;
    WantAllBtn: TSpeedButton;

    procedure ReportBtnClick(Sender: TObject);
    procedure T1BtnClick(Sender: TObject);
    procedure LogBtnClick(Sender: TObject);
    procedure StateBtnClick(Sender: TObject);
    procedure JsonBtnClick(Sender: TObject);

    procedure ReportListboxClick(Sender: TObject);
    procedure TrimmComboChange(Sender: TObject);
    procedure ParamComboChange(Sender: TObject);
    procedure M10BtnClick(Sender: TObject);
    procedure M1BtnClick(Sender: TObject);
    procedure P1BtnClick(Sender: TObject);
    procedure P10BtnClick(Sender: TObject);
    procedure SandboxedBtnClick(Sender: TObject);
    procedure CopyAndPasteBtnClick(Sender: TObject);
    procedure CopyTrimmFileBtnClick(Sender: TObject);
    procedure CopyTrimmItemBtnClick(Sender: TObject);
    procedure MT0BtnClick(Sender: TObject);
    procedure PasteTrimmItemBtnClick(Sender: TObject);
    procedure ReadTrimmFileBtnClick(Sender: TObject);
    procedure SaveTrimmFileBtnClick(Sender: TObject);
  protected
    Raster: Integer;
    Margin: Integer;
    BtnTop: Integer;
    BtnLeft: Integer;
    BtnWidth: Integer;
    BtnHeight: Integer;
    BtnCounter: Integer;
    BtnSpace: Integer;
    BtnGroupSpace: Integer;
    SpeedPanelHeight: Integer;
    BtnColor: TAlphaColor;
    function AddSpeedBtn(N: string; AGroupSpace: Integer = 0): TSpeedButton;
    function RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer = 0): TSpeedButton;

    procedure CreateComponents;
    procedure InitLayoutProps;
    procedure LayoutComponents;
    procedure InitSpeedButtons;
    procedure LinkComponents;
  protected
    function FindStyleByName(AParent: TFMXObject; AName: string): TFMXObject;
    procedure InitSpeedButton(SB: TSpeedButton);
    procedure SetupMemo(MM: TMemo);
    procedure SetupText(T: TText);
    procedure SetupComboBox(CB: TComboBox);
    procedure SetupListbox(LB: TListBox);
    procedure SetupListboxItems(LB: TListbox; cla: TAlphaColor);
  public
//    ReportManager: TRggReportManager;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  RiggVar.App.Main,
  RiggVar.FB.ActionConst,
  RiggVar.FB.Classes;

procedure TFormMain.FormCreate(Sender: TObject);
begin
{$ifdef Debug}
  ReportMemoryLeaksOnShutdown := True;
{$endif}
  FormatSettings.DecimalSeparator := '.';

  FormMain := self;
  Caption := 'RG14';
  Top := 20;
  Width := 1200;
  Height := 800;
  Margin := 10;
  Raster := 70;

  { RSP-20787 when TFormPosition.ScreenCenter}
//  Self.Position := TFormPosition.ScreenCenter;

  CreateComponents;
  InitLayoutProps;
  LayoutComponents;

  SetupListbox(ReportListbox);
  SetupMemo(TrimmMemo);
  SetupComboBox(TrimmCombo);
  SetupComboBox(ParamCombo);
  SetupMemo(ReportMemo);
  SetupMemo(LogMemo);
  SetupText(HintText);
  SetupText(ReportLabel);

  Main := TMain.Create;
  Main.Logger.Verbose := True;

  Main.RggMain.Init;
  Main.IsUp := True;

  InitSpeedButtons;
  LinkComponents;

  TL := TrimmMemo.Lines;
  RL := ReportMemo.Lines;
//  ReportManager := TRggReportManager.Create(ML);
//  ReportManager.InitLB(ReportListbox.Items);

  TrimmMemo.ShowScrollBars := False;
  TrimmMemo.Width := ReportListbox.Width;

  InitReportListbox;
  InitTrimmCombo;
  InitParamCombo;

  TrimmCombo.ItemIndex := 0;
  ParamCombo.ItemIndex := 0;
  ReportListbox.ItemIndex := 0;

  Main.Trimm := 1;
  MT0BtnClick(nil);
  ShowTrimm;

  HintText.BringToFront;
  HintText.TextSettings.FontColor := claPurple;
  ReportLabel.TextSettings.FontColor := claDodgerblue;

  UpdateLog;
  UpdateMemo;

  Application.OnHint := HandleShowHint;
  SetupListboxItems(ReportListbox, claDodgerblue);
  Self.OnResize := FormResize;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
//  ReportManager.Free;
  Main.Free;
  Main := nil;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  if Main <> nil then
    Inc(Main.ResizeCounter);
end;

procedure TFormMain.HandleShowHint(Sender: TObject);
begin
  HintText.Text := Application.Hint;
end;

procedure TFormMain.HandleAction(fa: TFederAction);
begin

end;

//function TFormMain.GetIsUp: Boolean;
//begin
//  result := (Main <> nil) and Main.IsUp;
//end;

function TFormMain.GetOpenFileName(dn, fn: string): string;
begin
  if not Assigned(OpenDialog) then
    OpenDialog := TOpenDialog.Create(self);

  OpenDialog.Options := [
    TOpenOption.ofPathMustExist,
    TOpenOption.ofFileMustExist,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  OpenDialog.Filter := 'Trimm-File|*.txt|Trimm-Datei|*.trm';
  OpenDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  OpenDialog.FileName := fn;

  if OpenDialog.Execute then
    result := OpenDialog.FileName
  else
    result := '';
end;

function TFormMain.GetSaveFileName(dn, fn: string): string;
begin
  if not Assigned(SaveDialog) then
    SaveDialog := TSaveDialog.Create(self);

  SaveDialog.Options := [
    TOpenOption.ofHideReadOnly,
    TOpenOption.ofPathMustExist,
    TOpenOption.ofNoReadOnlyReturn,
    TOpenOption.ofNoNetworkButton,
    TOpenOption.ofEnableSizing];
  SaveDialog.Filter := 'Trimm-File|*.txt|Trimm-Datei|*.trm';
  SaveDialog.InitialDir := ExcludeTrailingPathDelimiter(dn);
  SaveDialog.FileName := fn;

  if SaveDialog.Execute then
    result := SaveDialog.FileName
  else
    result := '';
end;

procedure TFormMain.SetupText(T: TText);
begin
  T.WordWrap := False;
  T.AutoSize := True;
  T.HorzTextAlign := TTextAlign.Leading;
  T.Font.Family := 'Consolas';
  T.Font.Size := 18;
end;

procedure TFormMain.SetupComboBox(CB: TComboBox);
begin
{$ifdef Vcl}
  CB.Style := csDropDownList;
  CB.DropDownCount := Integer(High(TFederParam));
  CB.Font.Name := 'Consolas';
  CB.Font.Size := 11;
  CB.Font.Color := clRed;
{$endif}
end;

procedure TFormMain.SetupListBox(LB: TListBox);
begin
{$ifdef Vcl}
  LB.Font.Name := 'Consolas';
  LB.Font.Size := 11;
  LB.Font.Color := clBlue;
{$endif}
end;

procedure TFormMain.SetupMemo(MM: TMemo);
begin
{$ifdef FMX}
  MM.ControlType := TControlType.Styled;
  MM.StyledSettings := [];
  MM.ShowScrollBars := True;
  MM.TextSettings.Font.Family := 'Consolas';
  MM.TextSettings.Font.Size := 14;
  MM.TextSettings.FontColor := claBlue;
{$endif}

{$ifdef Vcl}
  MM.Font.Name := 'Consolas';
  MM.Font.Size := 11;
  MM.Font.Color := clTeal;
  MM.ScrollBars := ssBoth;
{$endif}
end;

procedure TFormMain.UpdateLog;
begin
  LogMemo.Lines.Text := Main.Logger.TL.Text;
end;

procedure TFormMain.UpdateReport;
begin
  RL.Clear;
  Main.CurrentTrimm.WriteReport(RL);
end;

procedure TFormMain.UpdateJson;
begin
  RL.Clear;
  Main.CurrentTrimm.WriteJson(RL);
end;

procedure TFormMain.UpdateMemo;
begin
  UpdateJson;
end;

procedure TFormMain.ReportBtnClick(Sender: TObject);
begin
//  Main.DoCleanReport;
  Main.DoReport;
  UpdateLog;
end;

procedure TFormMain.LogBtnClick(Sender: TObject);
begin
  UpdateLog;
end;

procedure TFormMain.T1BtnClick(Sender: TObject);
var
  t: Integer;
begin
  t := (Sender as TComponent).Tag;
  if (t >= 1) and (t <= 8) then
  begin
    Main.Trimm := t;
    UpdateLog;
    UpdateMemo;
  end;
end;

procedure TFormMain.StateBtnClick(Sender: TObject);
begin
  //ML.Text := Main.TrimmData;
  UpdateReport;
end;

procedure TFormMain.JsonBtnClick(Sender: TObject);
begin
  UpdateJson;
end;

function TFormMain.AddSpeedBtn(N: string; AGroupSpace: Integer): TSpeedButton;
begin
  result := TSpeedButton.Create(SpeedPanel);
  result.Parent := SpeedPanel;
  result.Name := N;
  RefSpeedBtn(result, AGroupSpace);
end;

function TFormMain.RefSpeedBtn(B: TSpeedButton; AGroupSpace: Integer): TSpeedButton;
begin
  result := B;
  BtnLeft := BtnLeft + AGroupSpace;
{$ifdef Vcl}
  B.Left := BtnLeft + BtnCounter * BtnWidth + BtnSpace;
  B.Top := BtnTop;
{$endif}
{$ifdef FMX}
  B.Position.X := BtnLeft + BtnCounter * (BtnWidth + BtnSpace);
  B.Position.Y := BtnTop;
{$endif}
  B.Width := BtnWidth;
  B.Height := BtnHeight;
{$ifdef FMX}
  { Does not work.
    Because B not assigned yet to actual SpeedButton instance ? }
//  InitSpeedButton(B);
{$endif}
{$ifdef Vcl}
  B.Font.Name := 'Consolas';
  B.Font.Size := 12;
  B.Font.Color := BtnColor;
{$endif}
  Inc(BtnCounter);
end;

procedure TFormMain.InitSpeedButtons;
var
  sb: TSpeedButton;
begin
  { Report Buttons }

  BtnColor := claOrange;

  sb := AddSpeedBtn('LogBtn', BtnGroupSpace);
  LogBtn := sb;
  sb.Text := 'Log';
  sb.Hint := 'Log Btn';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := LogBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ReportBtn');
  ReportBtn := sb;
  sb.Text := 'Rep';
  sb.Hint := 'Report Btn';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := ReportBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('StateBtn');
  StateBtn := sb;
  sb.Text := 'Sta';
  sb.Hint := 'State Btn';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := StateBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('JsonBtn');
  StateBtn := sb;
  sb.Text := 'Jsn';
  sb.Hint := 'Json Btn';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := JsonBtnClick;
  InitSpeedButton(sb);

  { Data Buttons }

  BtnColor := claTeal;

  sb := AddSpeedBtn('MT0Btn', BtnGroupSpace);
  MT0Btn := sb;
  sb.Text := 'MT0';
  sb.Hint := 'Memory Btn';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := MT0BtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ReadTrimmFileBtn', BtnGroupSpace);
  ReadTrimmFileBtn := sb;
  sb.Text := 'rtf';
  sb.Hint := 'Read Trimm File';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := ReadTrimmFileBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('SaveTrimmFileBtn');
  SaveTrimmFileBtn := sb;
  sb.Text := 'stf';
  sb.Hint := 'Save Trimm File';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := SaveTrimmFileBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyTrimmItemBtn');
  CopyTrimmItemBtn := sb;
  sb.Text := 'cti';
  sb.Hint := 'Copy Trimm Item';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := CopyTrimmItemBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('PasteTrimmItemBtn');
  PasteTrimmItemBtn := sb;
  sb.Text := 'pti';
  sb.Hint := 'Paste Trimm Item';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := PasteTrimmItemBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyAndPasteBtn');
  CopyAndPasteBtn := sb;
  sb.Text := 'M';
  sb.Hint := 'Copy And Paste';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := CopyAndPasteBtnClick;
  InitSpeedButton(sb);

  { Param Value Button }

  BtnColor := claBlue;

  sb := AddSpeedBtn('M10Btn', BtnGroupSpace);
  M10Btn := sb;
  sb.Text := '-10';
  sb.Hint := 'Param Value Minus 10';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := M10BtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('M1Btn');
  M1Btn := sb;
  sb.Text := '-1';
  sb.Hint := 'Param Value Minus 1';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := M1BtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('P1Btn');
  P1Btn := sb;
  sb.Text := '+1';
  sb.Hint := 'Param Value Plus 1';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := P1BtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('P10Btn');
  P10Btn := sb;
  sb.Text := '+10';
  sb.Hint := 'Param Value Plus 10';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := P10BtnClick;
  InitSpeedButton(sb);

  { Check Box Buttons }

  BtnColor := claPurple;

  sb := AddSpeedBtn('SandboxedBtn', BtnGroupSpace);
  SandboxedBtn := sb;
  sb.Text := 'SB';
  sb.Hint := 'Sandboxed';
  sb.StaysPressed := True;
  sb.IsPressed := False;
  sb.OnClick := SandboxedBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('WantAllBtn');
  WantAllBtn := sb;
  sb.Text := 'WA';
  sb.Hint := 'Want All Xml Items';
  sb.StaysPressed := True;
  sb.IsPressed := False;
  InitSpeedButton(sb);

  { Trimm Buttons }

  BtnColor := claGreen;

  sb := AddSpeedBtn('T1Btn', BtnGroupSpace);
  T1Btn := sb;
  sb.Text := 'T1';
  sb.Hint := 'Trimm 1 Btn';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := T1BtnClick;
  sb.Tag := 1;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T2Btn', BtnGroupSpace);
  T2Btn := sb;
  sb.Text := 'T2';
  sb.Hint := 'Trimm 2 Btn';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := T1BtnClick;
  sb.Tag := 2;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T3Btn', BtnGroupSpace);
  T3Btn := sb;
  sb.Text := 'T3';
  sb.Hint := 'Trimm 3 Btn';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := T1BtnClick;
  sb.Tag := 3;
  InitSpeedButton(sb);

end;

procedure TFormMain.CopyTrimmItemBtnClick(Sender: TObject);
begin
  Main.CopyTrimmItem;
  UpdateLog;
end;

procedure TFormMain.PasteTrimmItemBtnClick(Sender: TObject);
begin
  Main.PasteTrimmItem;
  UpdateLog;
end;

procedure TFormMain.CopyAndPasteBtnClick(Sender: TObject);
begin
  Main.CopyAndPaste;
  UpdateLog;
end;

procedure TFormMain.CopyTrimmFileBtnClick(Sender: TObject);
begin
  Main.CopyTrimmFile;
  UpdateLog;
end;

procedure TFormMain.ReadTrimmFileBtnClick(Sender: TObject);
begin
  Main.ReadTrimmFile;
  UpdateLog;
end;

procedure TFormMain.SaveTrimmFileBtnClick(Sender: TObject);
begin
  Main.SaveTrimmFile;
  UpdateLog;
end;

procedure TFormMain.MT0BtnClick(Sender: TObject);
begin
  Main.UpdateTrimm0;
  //Main.FederText.UpdateText;
  UpdateLog;
end;

procedure TFormMain.SandboxedBtnClick(Sender: TObject);
begin
  IsSandboxed := SandboxedBtn.IsPressed;
end;

procedure TFormMain.M10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus10);
  ShowTrimm;
end;

procedure TFormMain.M1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus1);
  ShowTrimm;
end;

procedure TFormMain.P10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus10);
  ShowTrimm;
end;

procedure TFormMain.P1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus1);
  ShowTrimm;
end;

procedure TFormMain.InitReportListBox;
var
  ML: TStrings;
  r: TRggReport;
begin
  ML := ReportListbox.Items;

  for r := Low(TRggReport) to High(TRggReport) do
    ML.Add(GetReportCaption(r));
end;

procedure TFormMain.ReportListboxClick(Sender: TObject);
var
  ii: Integer;
begin
  RL.Clear;
  ii := ReportListbox.ItemIndex;
  if (ii >= 0) and (ii <= Integer(High(TRggReport)))then
  begin
    CurrentReport := TRggReport(ii);
    ShowCurrentReport;
  end;
end;

procedure TFormMain.ShowCurrentReport;
begin
  RL.BeginUpdate;
  RL.Clear;
  case CurrentReport of
    rgLog: RL.Text := Main.Logger.TL.Text;
    rgJson: Main.RggData.WriteJSon(RL);
    rgData: Main.RggData.WriteReport(RL);
//    rgAusgabeRL:
//    begin
//      RiggReport.ML.Clear;
//      RiggReport.AusgabeRL(Main.RggMain.Rigg.rL);
//      ML.Assign(RiggReport.ML);
//    end;
//    rgAusgabeRP:
//    begin
//      RiggReport.ML.Clear;
//      RiggReport.AusgabeRP(Main.RggMain.Rigg.rP);
//      ML.Assign(RiggReport.ML);
//    end;
//    rgXML: Main.RggMain.Rigg.WriteXml(ML);
//    rgDiffText: Main.RggMain.UpdateDiffText(ML);
//    rgDataText: Main.RggMain.UpdateDataText(ML);
//    rgTrimmText: Main.RggMain.UpdateTrimmText(ML);
    rgDebugReport:
    begin
      Main.DoCleanReport;
      RL.Text := Main.Logger.TL.Text;
    end;
  end;
  RL.EndUpdate;

  ReportLabel.Text := GetReportCaption(CurrentReport)
end;

procedure TFormMain.ACI(fp: TFederParam);
var
  s: string;
begin
  s := Main.RggMain.Param2Text(fp);
  ParamCombo.Items.AddObject(s, TObject(fp));
end;

procedure TFormMain.InitParamCombo;
begin
  ACI(fpVorstag);
  ACI(fpWinkel);
  ACI(fpController);
  ACI(fpWante);
  ACI(fpWoben);
  ACI(fpSalingH);
  ACI(fpSalingA);
  ACI(fpSalingL);
  ACI(fpSalingW);
  ACI(fpMastfallF0C);
  ACI(fpMastfallF0F);
  ACI(fpMastfallVorlauf);
  ACI(fpBiegung);
  ACI(fpD0X);
end;

procedure TFormMain.InitTrimmCombo;
var
  cl: TStrings;
begin
  cl := TrimmCombo.Items;
  cl.AddObject('Trimm1', TObject(1));
  cl.AddObject('Trimm2', TObject(2));
  cl.AddObject('Trimm3', TObject(3));
  cl.AddObject('Trimm4', TObject(4));
  cl.AddObject('Trimm5', TObject(5));
  cl.AddObject('Trimm6', TObject(6));
  cl.AddObject('Trimm7 (420)', TObject(7));
  cl.AddObject('Trimm8 (Logo)', TObject(8));
end;

procedure TFormMain.TrimmComboChange(Sender: TObject);
var
  t: Integer;
  ii: Integer;
begin
  ii := TrimmCombo.ItemIndex;
  t := Integer(TrimmCombo.Items.Objects[ii]);
  Main.Trimm := t;

  RL.BeginUpdate;
  try
    RL.Clear;

    //Main.CurrentTrimm.SaveTrimmFile(ML);

    Main.CurrentTrimm.WantAll := WantAllBtn.IsPressed;
    Main.CurrentTrimm.SaveTrimmItem(RL);
    Main.CurrentTrimm.WantAll := False;

    //Main.CurrentTrimm.WriteReport(ML);

    ReportLabel.Text := 'Trimm' + IntToStr(t);
  finally
    RL.EndUpdate;
  end;
end;

procedure TFormMain.ParamComboChange(Sender: TObject);
var
  ii: Integer;
  fp: TFederParam;
begin
  ii := ParamCombo.ItemIndex;
  fp := TFederParam(ParamCombo.Items.Objects[ii]);
  Main.RggMain.Param := fp;
  ShowTrimm;
end;

procedure TFormMain.ShowTrimm;
begin
  if TL <> nil then
    Main.RggMain.UpdateTrimmText(TL);
  ShowCurrentReport;
end;

procedure TFormMain.InitLayoutProps;
begin
  BtnCounter := 0;
  BtnLeft := 0;
  BtnTop := 3;
  BtnSpace := 2;
  BtnGroupSpace := 16;
  BtnWidth := 35;
  BtnHeight := 30;
  BtnColor := claBlue;
  SpeedPanelHeight := BtnHeight + 2 * BtnTop;
end;

function TFormMain.GetReportCaption(r: TRggReport): string;
begin
  case r of
    rgLog: result := 'Log';
    rgJson: result := 'Main.RggData.WriteJson';
    rgData: result := 'Main.RggData.WriteReport';
    rgTrimmText: result := 'Trimm Text';
    rgDataText: result := 'Data Text';
    rgDiffText: result := 'Diff Text';
    rgAusgabeRL: result := 'Ausgabe rL';
    rgAusgabeRP: result := 'Ausgabe rP';
    rgXML: result := 'Write XML';
    rgDebugReport: result := 'Debug Report';
    else
      result := 'Unknown';
  end;
end;

procedure TFormMain.MemoMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if ssShift in Shift then
    Main.RggMain.DoBigWheel(WheelDelta)
  else if not (ssCtrl in Shift) then
    Main.RggMain.DoSmallWheel(WheelDelta);

  if (ssShift in Shift) or (ssCtrl in Shift) then
  begin
    Main.DoMouseWheel(Shift, WheelDelta);
    ShowTrimm;
    Handled := True;
  end;
end;

procedure TFormMain.CreateComponents;
var
  OpacityValue: single;
begin
  OpacityValue := 1.0;

  HintText := TText.Create(Self);
  HintText.Parent := Self;
//  HintText.WordWrap := False;
//  HintText.AutoSize := True;
//  HintText.HorzTextAlign := TTextAlign.Leading;
//  HintText.Font.Family := 'Consolas';
//  HintText.Font.Size := 18;

  ReportLabel := TText.Create(Self);
  ReportLabel.Parent := Self;

//  HelpText := TText.Create(Self);
//  HelpText.Parent := Self;
//  HelpText.WordWrap := False;
//  HelpText.HorzTextAlign := TTextAlign.Leading;
//  HelpText.Font.Family := 'Courier New';
//  HelpText.Font.Size := 16;
//  HelpText.AutoSize := True;
//
//  ReportText := TText.Create(Self);
//  ReportText.Parent := Self;
//  ReportText.WordWrap := False;
//  ReportText.HorzTextAlign := TTextAlign.Leading;
//  ReportText.Font.Family := 'Courier New';
//  ReportText.Font.Size := 16;
//  ReportText.AutoSize := True;

  SpeedPanel := TPanel.Create(Self);
  SpeedPanel.Parent := Self;
  SpeedPanel.ShowHint := True;
  SpeedPanel.Opacity := OpacityValue;

  TrimmMemo := TMemo.Create(Self);
  TrimmMemo.Parent := Self;
  TrimmMemo.ReadOnly := True;
  TrimmMemo.CanFocus := False;
  TrimmMemo.Opacity := OpacityValue;

  TrimmCombo := TComboBox.Create(Self);
  TrimmCombo.Parent := Self;

  ParamCombo := TComboBox.Create(Self);
  ParamCombo.Parent := Self;

  ReportListbox := TListbox.Create(Self);
  ReportListbox.Parent := Self;
  ReportListbox.Opacity := OpacityValue;

  LogMemo := TMemo.Create(Self);
  LogMemo.Parent := Self;
  LogMemo.ReadOnly := True;
  LogMemo.CanFocus := False;
  LogMemo.Opacity := OpacityValue;

  ReportMemo := TMemo.Create(Self);
  ReportMemo.Parent := Self;
  ReportMemo.ReadOnly := True;
  ReportMemo.CanFocus := False;
  ReportMemo.Opacity := OpacityValue;

//  Bitmap := TBitmap.Create(1024, 768);

//  Image := TImage.Create(Self);
//  Image.Parent := Self;
//  Image.Bitmap := Bitmap;
//  Image.WrapMode := TImageWrapMode.Original;

//  MT0Btn := TSpeedButton.Create(Self);
//  ReadTrimmFileBtn := TSpeedButton.Create(Self);
//  SaveTrimmFileBtn := TSpeedButton.Create(Self);
//  PasteTrimmItemBtn := TSpeedButton.Create(Self);
//  CopyTrimmItemBtn := TSpeedButton.Create(Self);
//  CopyAndPasteBtn := TSpeedButton.Create(Self);
//
//  M10Btn := TSpeedButton.Create(Self);
//  M1Btn := TSpeedButton.Create(Self);
//  P1Btn := TSpeedButton.Create(Self);
//  P10Btn := TSpeedButton.Create(Self);
//
//  SandboxedBtn := TSpeedButton.Create(Self);
//  WantAllBtn := TSpeedButton.Create(Self);
end;

procedure TFormMain.LayoutComponents;
var
  ComboHeight: single;
begin
  SpeedPanel.Align := TAlignLayout.Top;
  SpeedPanel.Height := SpeedPanelHeight;

  HintText.Position.X := 20;
  HintText.Position.Y := SpeedPanelHeight + Margin;
  HintText.Height := 30;

  ReportLabel.Position.X := 400;
  ReportLabel.Position.Y := SpeedPanelHeight + Margin;
  ReportLabel.Height := 30;

  TrimmMemo.Position.X := Margin;
  TrimmMemo.Position.Y := SpeedPanel.Height + HintText.Height + Margin;
  TrimmMemo.Height := 185;
  TrimmMemo.Width := 200;

  TrimmCombo.Position.X := TrimmMemo.Position.X;
  ParamCombo.Position.X := TrimmCombo.Position.X;

  TrimmCombo.Width := TrimmMemo.Width;
  ParamCombo.Width := TrimmCombo.Width;

  ComboHeight := TrimmCombo.Height + Margin;
  TrimmCombo.Position.Y := TrimmMemo.Position.Y + TrimmMemo.Height + Margin;
  ParamCombo.Position.Y := TrimmCombo.Position.Y + ComboHeight;

  ReportListbox.Position.X := TrimmMemo.Position.X;
  ReportListbox.Position.Y := ParamCombo.Position.Y + ComboHeight;
  ReportListbox.Width := TrimmMemo.Width;
  ReportListbox.Height := ClientHeight - ReportListbox.Position.Y - Margin;
  ReportListbox.Anchors := ReportListbox.Anchors + [TAnchorKind.akBottom];

  LogMemo.Position.X := ReportListbox.Position.X + ReportListbox.Width + Margin;
  LogMemo.Position.Y := TrimmMemo.Position.Y;
  LogMemo.Height := ClientHeight - LogMemo.Position.Y - Margin;
  LogMemo.Width := 400;
  LogMemo.Anchors := LogMemo.Anchors + [TAnchorKind.akBottom];

  ReportMemo.Position.X := LogMemo.Position.X + LogMemo.Width + Margin;
  ReportMemo.Position.Y := TrimmMemo.Position.Y;
  ReportMemo.Height := ClientHeight - ReportMemo.Position.Y - Margin;
  ReportMemo.Width := ClientWidth - ReportMemo.Position.X - Margin;
  ReportMemo.Anchors := ReportMemo.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];

//  Image.Position.X := ReportMemo.Position.X + ReportMemo.Width + Margin;
//  Image.Position.Y := SpeedPanel.Position.Y + SpeedPanel.Height + Margin;
//  Image.Width := ClientWidth - Image.Position.X - Margin;
//  Image.Height := ClientHeight - Image.Position.Y - Margin;
//  Image.Anchors := Image.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];
end;

procedure TFormMain.LinkComponents;
begin
//  MT0Btn.OnClick := MT0BtnClick;
//  CopyAndPasteBtn.OnClick := CopyAndPasteBtnClick;
//  PasteTrimmItemBtn.OnClick := PasteTrimmItemBtnClick;
//  CopyTrimmItemBtn.OnClick := CopyTrimmItemBtnClick;
//  ReadTrimmFileBtn.OnClick := ReadTrimmFileBtnClick;
//  SaveTrimmFileBtn.OnClick := SaveTrimmFileBtnClick;
//
//  M1Btn.OnClick := M1BtnClick;
//  M10Btn.OnClick := M10BtnClick;
//  P1Btn.OnClick := P1BtnClick;
//  P10Btn.OnClick := P10BtnClick;
//  SandboxedBtn.OnClick := SandboxedBtnClick;

  ReportListbox.OnClick := ReportListboxClick;
  ReportListbox.OnChange := ReportListboxClick;

  TrimmCombo.OnChange := TrimmComboChange;
  ParamCombo.OnChange := ParamComboChange;

  ReportMemo.OnMouseWheel := MemoMouseWheel;
end;

procedure TFormMain.SetupListboxItems(LB: TListbox; cla: TAlphaColor);
var
  i: Integer;
  cr: TListBoxItem;
  T: TText;
begin
  if LB.Items.Count > 0 then
  for i := 0 to LB.Items.Count - 1 do
  begin
    cr := LB.ItemByIndex(i);
    T := cr.FindStyleResource('text') as TText;
    if Assigned(T) then
    begin
      T.Font.Family := 'Consolas';
      T.Font.Size := 14;
      T.TextSettings.FontColor := cla;
    end;
  end;
end;

procedure TFormMain.InitSpeedButton(SB: TSpeedButton);
var
  cr: TButtonStyleTextObject;
begin
  cr := FindStyleByName(SB, 'text') as TButtonStyleTextObject;
  cr.NormalColor := BtnColor;
  cr.PressedColor := BtnColor;
  cr.Font.Family := 'Consolas';
  cr.Font.Size := 16;
end;

function TFormMain.FindStyleByName(AParent: TFMXObject; AName: string): TFMXObject;
var
  i: Integer;
  AObj: TFMXObject;
begin
  result := nil;
  for i := 0 to AParent.ChildrenCount - 1 do
  begin
    AObj := AParent.Children[i];
    if AObj.StyleName = AName then
      Result := AObj
    else
      Result := FindStyleByName(AObj, AName);
    if Assigned(result) then
      break;
  end;
end;

end.
