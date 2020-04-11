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
  RggUnit4,
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
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  protected
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure UpdateLog;
    procedure UpdateReport;
    procedure UpdateJson;
  private
    RL: TStrings;
    AllProps: Boolean;
    CurrentReport: TRggReport;
    Counter: Integer;
    procedure ShowCurrentReport;
    procedure ShowTrimmData;
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
    T4Btn: TSpeedButton;
    T5Btn: TSpeedButton;
    T6Btn: TSpeedButton;
    T7Btn: TSpeedButton;
    T8Btn: TSpeedButton;

    LogBtn: TSpeedButton;
    ShortBtn: TSpeedButton;
    LongBtn: TSpeedButton;
    StateBtn: TSpeedButton;
    JsonBtn: TSpeedButton;
  public
    HintText: TText;
    SpeedPanel: TPanel;
    ReportLabel: TText;
    ReportMemo: TMemo;
    LogMemo: TMemo;

    MT0Btn: TSpeedButton;

    ReadTrimmFileBtn: TSpeedButton;
    SaveTrimmFileBtn: TSpeedButton;
    CopyTrimmFileBtn: TSpeedButton;

    CopyTrimmItemBtn: TSpeedButton;
    PasteTrimmItemBtn: TSpeedButton;
    CopyAndPasteBtn: TSpeedButton;

    SandboxedBtn: TSpeedButton;

    procedure LogBtnClick(Sender: TObject);
    procedure ShortBtnClick(Sender: TObject);
    procedure LongBtnClick(Sender: TObject);
    procedure StateBtnClick(Sender: TObject);
    procedure JsonBtnClick(Sender: TObject);
    procedure SandboxedBtnClick(Sender: TObject);
    procedure ReadTrimmFileBtnClick(Sender: TObject);
    procedure SaveTrimmFileBtnClick(Sender: TObject);
    procedure CopyTrimmFileBtnClick(Sender: TObject);
    procedure CopyTrimmItemBtnClick(Sender: TObject);
    procedure PasteTrimmItemBtnClick(Sender: TObject);
    procedure CopyAndPasteBtnClick(Sender: TObject);
    procedure MT0BtnClick(Sender: TObject);
    procedure TrimmBtnClick(Sender: TObject);
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
    function FindStyleByName(AParent: TFMXObject; AName: string): TFMXObject;
    procedure InitSpeedButton(SB: TSpeedButton);
    procedure SetupMemo(MM: TMemo);
    procedure SetupText(T: TText);
    procedure SetupComboBox(CB: TComboBox);
    procedure SetupListbox(LB: TListBox);
    procedure SetupListboxItems(LB: TListbox; cla: TAlphaColor);
  private
    Rigg: TRigg;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  RiggVar.App.Main,
  RiggVar.FB.ActionConst;

const
  ApplicationTitleText = 'RG14';

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  if (Main <> nil) and (Main.Logger <> nil) then
    Main.Logger.Info(E.Message);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
{$ifdef Debug}
  ReportMemoryLeaksOnShutdown := True;
{$endif}
  FormatSettings.DecimalSeparator := '.';

  Application.OnException := ApplicationEventsException;

  FormMain := self;
  Caption := ApplicationTitleText;
  Top := 20;
  Width := 1200;
  Height := 800;
  Margin := 10;
  Raster := 70;

  CreateComponents;
  InitLayoutProps;
  LayoutComponents;

  SetupMemo(ReportMemo);
  SetupMemo(LogMemo);
  SetupText(HintText);
  SetupText(ReportLabel);

  Main := TMain.Create;
  Main.Logger.Verbose := True;

  Main.RggMain.Init;
  Main.IsUp := True;
  Rigg := Main.RggMain.Rigg;

  InitSpeedButtons;

  RL := ReportMemo.Lines;

  Main.Trimm := 1;
  MT0BtnClick(nil);

  HintText.BringToFront;
  HintText.TextSettings.FontColor := claPurple;

  ReportLabel.TextSettings.FontColor := claDodgerblue;

  UpdateLog;
  CurrentReport := rgJson;
  ShowCurrentReport;

  Application.OnHint := HandleShowHint;
  self.OnResize := FormResize;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Main.Free;
  Main := nil;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  //
end;

procedure TFormMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  if ssShift in Shift then
  begin
    Main.RggMain.DoSmallWheel(WheelDelta);
    ShowCurrentReport;
  end
  else if ssCtrl in Shift then
  begin
    Main.RggMain.DoBigWheel(WheelDelta);
    ShowCurrentReport;
  end;
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

procedure TFormMain.HandleAction(fa: Integer);
begin

end;

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
  T.HorzTextAlign := TTextAlign.Leading;
  T.Font.Family := 'Consolas';
  T.Font.Size := 18;
  T.AutoSize := True;
end;

procedure TFormMain.SetupComboBox(CB: TComboBox);
begin
  if CB = nil then
    Exit;

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
  if LB = nil then
    Exit;

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

procedure TFormMain.LogBtnClick(Sender: TObject);
begin
  Main.DoCleanReport;
//  Main.DoReport;

  UpdateLog;
end;

procedure TFormMain.ShortBtnClick(Sender: TObject);
begin
  AllProps := False;
  ShowTrimmData;
  ReportLabel.Text := 'Short Data';
end;

procedure TFormMain.LongBtnClick(Sender: TObject);
begin
  AllProps := True;
  ShowTrimmData;
  ReportLabel.Text := 'Long Data';
end;

procedure TFormMain.StateBtnClick(Sender: TObject);
begin
  CurrentReport := rgData;
  ShowCurrentReport;
end;

procedure TFormMain.JsonBtnClick(Sender: TObject);
begin
  CurrentReport := rgJson;
  ShowCurrentReport;
end;

procedure TFormMain.TrimmBtnClick(Sender: TObject);
var
  t: Integer;
begin
  t := (Sender as TComponent).Tag;
  if (t >= 1) and (t <= 8) then
  begin
    Main.Trimm := t;
    UpdateLog;
    ShowCurrentReport;
  end;
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
  { update log }

  BtnColor := claOrange;

  sb := AddSpeedBtn('LogBtn', BtnGroupSpace);
  LogBtn := sb;
  sb.Text := 'Log';
  sb.Hint := 'Log Btn';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := LogBtnClick;
  InitSpeedButton(sb);

  { Report Buttons }

  BtnColor := claCrimson;

  sb := AddSpeedBtn('ShortBtn', BtnGroupSpace);
  ShortBtn := sb;
  sb.Text := 'dat';
  sb.Hint := 'Data (Short)';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := ShortBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('LongBtn');
  LongBtn := sb;
  sb.Text := 'DAT';
  sb.Hint := 'Data (Long)';
  sb.StaysPressed := False;
  sb.OnClick := LongBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('StateBtn');
  StateBtn := sb;
  sb.Text := 'Sta';
  sb.Hint := 'State Btn';
  sb.StaysPressed := False;
  sb.OnClick := StateBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('JsonBtn');
  StateBtn := sb;
  sb.Text := 'Jsn';
  sb.Hint := 'Json Btn';
  sb.StaysPressed := False;
  sb.OnClick := JsonBtnClick;
  InitSpeedButton(sb);

  { Option Buttons }

  BtnColor := claPurple;

  sb := AddSpeedBtn('SandboxedBtn', BtnGroupSpace);
  SandboxedBtn := sb;
  sb.Text := 'SB';
  sb.Hint := 'Sandboxed';
  sb.StaysPressed := True;
  sb.OnClick := SandboxedBtnClick;
  InitSpeedButton(sb);

  { Data Buttons }

  BtnColor := claTeal;

  sb := AddSpeedBtn('MT0Btn', BtnGroupSpace);
  MT0Btn := sb;
  sb.Text := 'MT0';
  sb.Hint := 'Memory Btn';
  sb.StaysPressed := False;
  sb.OnClick := MT0BtnClick;
  sb.Tag := faUpdateTrimm0;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ReadTrimmFileBtn');
  ReadTrimmFileBtn := sb;
  sb.Text := 'rtf';
  sb.Hint := 'Read Trimm File';
  sb.StaysPressed := False;
  sb.OnClick := ReadTrimmFileBtnClick;
  sb.Tag := faReadTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('SaveTrimmFileBtn');
  SaveTrimmFileBtn := sb;
  sb.Text := 'stf';
  sb.Hint := 'Save Trimm File';
  sb.StaysPressed := False;
  sb.OnClick := SaveTrimmFileBtnClick;
  sb.Tag := faSaveTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyTrimmFileBtn');
  CopyTrimmFileBtn := sb;
  sb.Text := 'ctf';
  sb.Hint := 'Copy Trimm File';
  sb.StaysPressed := False;
  sb.OnClick := CopyTrimmFileBtnClick;
  sb.Tag := faCopyTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyTrimmItemBtn');
  CopyTrimmItemBtn := sb;
  sb.Text := 'cti';
  sb.Hint := 'Copy Trimm Item';
  sb.StaysPressed := False;
  sb.OnClick := CopyTrimmItemBtnClick;
  sb.Tag := faCopyTrimmItem;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('PasteTrimmItemBtn');
  PasteTrimmItemBtn := sb;
  sb.Text := 'pti';
  sb.Hint := 'Paste Trimm Item';
  sb.StaysPressed := False;
  sb.OnClick := PasteTrimmItemBtnClick;
  sb.Tag := faPasteTrimmItem;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyAndPasteBtn');
  CopyAndPasteBtn := sb;
  sb.Text := 'cap';
  sb.Hint := 'Copy And Paste Trimm Item';
  sb.StaysPressed := False;
  sb.OnClick := CopyAndPasteBtnClick;
  sb.Tag := faCopyAndPaste;
  InitSpeedButton(sb);

  { Trimm Buttons }

  BtnColor := claBlue;

  sb := AddSpeedBtn('T1Btn', BtnGroupSpace);
  T1Btn := sb;
  sb.Text := 'T1';
  sb.Hint := 'Trimm 1 Btn';
  sb.StaysPressed := False;
  sb.OnClick := TrimmBtnClick;
  sb.Tag := 1;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T2Btn');
  T2Btn := sb;
  sb.Text := 'T2';
  sb.Hint := 'Trimm 2 Btn';
  sb.StaysPressed := False;
  sb.OnClick := TrimmBtnClick;
  sb.Tag := 2;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T3Btn');
  T3Btn := sb;
  sb.Text := 'T3';
  sb.Hint := 'Trimm 3 Btn';
  sb.StaysPressed := False;
  sb.OnClick := TrimmBtnClick;
  sb.Tag := 3;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T4Btn');
  T4Btn := sb;
  sb.Text := 'T4';
  sb.Hint := 'Trimm 4 Btn';
  sb.StaysPressed := False;
  sb.OnClick := TrimmBtnClick;
  sb.Tag := 4;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T5Btn');
  T5Btn := sb;
  sb.Text := 'T5';
  sb.Hint := 'Trimm 5 Btn';
  sb.StaysPressed := False;
  sb.IsPressed := False;
  sb.OnClick := TrimmBtnClick;
  sb.Tag := 5;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T6Btn');
  T6Btn := sb;
  sb.Text := 'T6';
  sb.Hint := 'Trimm 6 Btn';
  sb.StaysPressed := False;
  sb.OnClick := TrimmBtnClick;
  sb.Tag := 6;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T7Btn');
  T7Btn := sb;
  sb.Text := 'T7';
  sb.Hint := 'Trimm 7 Btn (420)';
  sb.StaysPressed := False;
  sb.OnClick := TrimmBtnClick;
  sb.Tag := 7;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T8Btn');
  T8Btn := sb;
  sb.Text := 'T8';
  sb.Hint := 'Trimm 8 Btn (Logo)';
  sb.StaysPressed := False;
  sb.OnClick := TrimmBtnClick;
  sb.Tag := 8;
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
  ShowCurrentReport;
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
  ShowCurrentReport;
end;

procedure TFormMain.SaveTrimmFileBtnClick(Sender: TObject);
begin
  Main.SaveTrimmFile;
  UpdateLog;
end;

procedure TFormMain.MT0BtnClick(Sender: TObject);
begin
  Main.UpdateTrimm0;
  UpdateLog;
  ShowCurrentReport;
end;

procedure TFormMain.SandboxedBtnClick(Sender: TObject);
begin
  IsSandboxed := SandboxedBtn.IsPressed;
end;

function TFormMain.GetReportCaption(r: TRggReport): string;
begin
  case r of
    rgLog: result := 'Log';
    rgJson: result := 'Rigg.Data.WriteJson';
    rgData: result := 'Rigg.Data.WriteReport';
    rgTrimmText: result := 'Trimm Text';
    rgJsonText: result := 'Json Text';
    rgDataText: result := 'Data Text';
    rgDiffText: result := 'Diff Text';
    rgAusgabeRL: result := 'Ausgabe rL';
    rgAusgabeRP: result := 'Ausgabe rP';
    rgXML: result := 'Write XML';
    rgDebugReport: result := 'Debug Report';
    else
      result := 'Unknown';
  end;
  result := Format('%s (%d)', [result, Counter]);
end;

procedure TFormMain.ShowCurrentReport;
begin
  Inc(Counter);
  RL.BeginUpdate;
  RL.Clear;
  case CurrentReport of
    rgLog: RL.Text := Main.Logger.TL.Text;
    rgJson: Rigg.Data.WriteJSon(RL);
    rgData: Rigg.Data.WriteReport(RL);
    rgDebugReport:
    begin
      Main.DoCleanReport;
      RL.Text := Main.Logger.TL.Text;
    end;
  end;
  RL.EndUpdate;
  ReportLabel.Text := GetReportCaption(CurrentReport)
end;

procedure TFormMain.ShowTrimmData;
begin
  RL.BeginUpdate;
  try
    RL.Clear;
    Main.CurrentTrimm.WantAll := AllProps;
    Main.CurrentTrimm.SaveTrimmItem(RL);
    Main.CurrentTrimm.WantAll := False;
  finally
    RL.EndUpdate;
  end;
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

procedure TFormMain.CreateComponents;
var
  OpacityValue: single;
begin
  OpacityValue := 1.0;

  HintText := TText.Create(Self);
  HintText.Parent := Self;

  ReportLabel := TText.Create(Self);
  ReportLabel.Parent := Self;

  SpeedPanel := TPanel.Create(Self);
  SpeedPanel.Parent := Self;
  SpeedPanel.ShowHint := True;
  SpeedPanel.Opacity := OpacityValue;

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
end;

procedure TFormMain.LayoutComponents;
begin
  SpeedPanel.Align := TAlignLayout.Top;
  SpeedPanel.Height := SpeedPanelHeight;

  HintText.Position.X := 20;
  HintText.Position.Y := SpeedPanelHeight + Margin;
  HintText.Height := 30;

  ReportLabel.Position.X := 400;
  ReportLabel.Position.Y := SpeedPanelHeight + Margin;
  ReportLabel.Height := 30;

  LogMemo.Position.X := Margin;
  LogMemo.Position.Y := SpeedPanel.Height + HintText.Height + Margin;;
  LogMemo.Height := ClientHeight - LogMemo.Position.Y - Margin;
  LogMemo.Width := 400;
  LogMemo.Anchors := LogMemo.Anchors + [TAnchorKind.akBottom];

  ReportMemo.Position.X := LogMemo.Position.X + LogMemo.Width + Margin;
  ReportMemo.Position.Y := LogMemo.Position.Y;
  ReportMemo.Height := ClientHeight - ReportMemo.Position.Y - Margin;
  ReportMemo.Width := ClientWidth - ReportMemo.Position.X - Margin;
  ReportMemo.Anchors := ReportMemo.Anchors + [TAnchorKind.akRight, TAnchorKind.akBottom];
end;

procedure TFormMain.SetupListboxItems(LB: TListbox; cla: TAlphaColor);
var
  i: Integer;
  cr: TListBoxItem;
  T: TText;
begin
  if LB = nil then
    Exit;
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
