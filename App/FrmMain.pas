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
  RiggVar.FB.SpeedBar,
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
  FMX.Dialogs;

{$define FMX}

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  private
    FormShown: Boolean;
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure HandleShowHint(Sender: TObject);
  protected
    RL: TStrings;
  public
    AllProps: Boolean;
    procedure ShowTrimm;
    procedure ShowTrimmData;
  public
    FWantButtonFrameReport: Boolean;
    BackgroundColor: TAlphaColor;
    procedure UpdateLog;
    procedure UpdateReport;
    procedure ShowCurrentReport;
    property WantButtonFrameReport: Boolean read FWantButtonFrameReport;
  public
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
  public
    HintText: TText;
    ReportLabel: TText;
    TrimmText: TText;
    ReportMemo: TMemo;
    LogMemo: TMemo;
  protected
    procedure CreateComponents;
    procedure LayoutComponents;
    procedure SetupMemo(MM: TMemo);
    procedure SetupText(T: TText);
    procedure SetupComboBox(CB: TComboBox);
    procedure SetupListbox(LB: TListBox);
  private
    Raster: Integer;
    Margin: Integer;
    SpeedPanelHeight: Integer;
    SpeedPanel: TActionSpeedBar;
    procedure InitSpeedButtons;
    procedure UpdateSpeedButtonDown;
    procedure UpdateSpeedButtonEnabled;
  public
    procedure HandleAction(fa: Integer);
  public
    Rigg: TRigg;
    ReportManager: TRggReportManager;
    function GetIsUp: Boolean;
    property IsUp: Boolean read GetIsUp;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  RiggVar.RG.Main,
  RiggVar.App.Main,
  RiggVar.RG.Speed01,
  RiggVar.FB.ActionConst;

const
  ApplicationTitleText = 'RG14';

procedure TFormMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  if (Main <> nil) and (Main.Logger <> nil) then
    Main.Logger.Info(E.Message);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  rggm: TRggMain;
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
  SpeedPanelHeight := 40;

  CreateComponents;

  SetupMemo(ReportMemo);
  SetupMemo(LogMemo);
  SetupText(ReportLabel);

  Rigg := TRigg.Create;

  rggm := TRggMain.Create(Rigg); // rggm owns Rigg
  Main := TMain.Create(rggm); // Main owns rggm
  Main.Logger.Verbose := True;
  rggm.InitLogo; // sets WantLogoData to true
  rggm.Init420; // resets WantLogoData to false

  { Reports }
  RL := TStringList.Create;
  ReportManager := TRggReportManager.Create(RL);
  ReportManager.CurrentReport := rgJson;

  HintText.BringToFront;
  HintText.TextSettings.FontColor := claYellow;

  ReportLabel.BringToFront;
  ReportLabel.TextSettings.FontColor := claOrange;

  TrimmText.BringToFront;
  TrimmText.TextSettings.FontColor := claChartreuse;
  TrimmText.Visible := True;

  Main.IsUp := True;
  Main.Trimm := 1;
  Main.UpdateTrimm0;
  ShowTrimm;

  { Background }
  BackgroundColor := claGray;
  Fill.Kind := TBrushKind.Solid;
  Fill.Color := BackgroundColor;

  Application.OnHint := HandleShowHint;
  InitSpeedButtons;
  UpdateSpeedButtonDown;
  UpdateSpeedButtonEnabled;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  RL.Free;
  ReportManager.Free;
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
    Handled := True;
  end
  else if ssCtrl in Shift then
  begin
    Main.RggMain.DoBigWheel(WheelDelta);
    Handled := True;
  end;

  if Handled then
  begin
    if ReportManager.CurrentReport = rgLong then
       ReportManager.CurrentReport := rgShort;
    ShowCurrentReport;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if not FormShown then
  begin
    FormShown := True;
    { ClientHeigt is now available }
    LayoutComponents;
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  MainVar.ClientWidth := ClientWidth;
  MainVar.ClientHeight := ClientHeight;
  if (Main <> nil) and Main.IsUp then
  begin
    Inc(Main.ResizeCounter);
  end;
end;

procedure TFormMain.HandleShowHint(Sender: TObject);
begin
  HintText.Text := Application.Hint;
end;

procedure TFormMain.HandleAction(fa: Integer);
begin

end;

function TFormMain.GetIsUp: Boolean;
begin
  result := (Main <> nil) and Main.IsUp;
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
  T.Parent := Self;
  T.WordWrap := False;
  T.HorzTextAlign := TTextAlign.Leading;
  T.Font.Family := 'Consolas';
  T.Font.Size := 18;
  T.AutoSize := True;
  T.HitTest := False;
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
  if MM = nil then
    Exit;

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
  if not ReportMemo.Visible then
    Exit;
  if ReportManager = nil then
    Exit;
  if RL = nil then
    Exit;
  if not IsUp then
    Exit;

  RL.Clear;

  if WantButtonFrameReport then
  begin
//    Main.FederText.Report(RL);
  end
  else
  begin
    ReportManager.ShowCurrentReport;
  end;

  ReportMemo.Text := RL.Text;
end;

procedure TFormMain.ShowCurrentReport;
begin
  ReportManager.ShowCurrentReport;
  ReportLabel.Text := ReportManager.GetReportCaption(ReportManager.CurrentReport);
  ReportMemo.Text := RL.Text;
  TrimmText.Text := 'Trimm ' + IntToStr(Main.Trimm);
end;

procedure TFormMain.ShowTrimm;
begin
  ShowCurrentReport;
  UpdateLog;
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
  ReportMemo.Text := RL.Text;
end;

procedure TFormMain.CreateComponents;
var
  OpacityValue: single;
begin
  OpacityValue := 1.0;

  HintText := TText.Create(Self);
  SetupText(HintText);

  ReportLabel := TText.Create(Self);
  ReportLabel.Parent := Self;

  TrimmText := TText.Create(Self);
  SetupText(TrimmText);

  SpeedPanel := TActionSpeedBarRG01.Create(Self);
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

  TrimmText.Position.X := 700;
  TrimmText.Position.Y := SpeedPanelHeight + Margin;
  TrimmText.Height := 30;

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

procedure TFormMain.InitSpeedButtons;
begin
  if SpeedPanel <> nil then
    SpeedPanel.InitSpeedButtons;
end;

procedure TFormMain.UpdateSpeedButtonDown;
begin
  if SpeedPanel <> nil then
    SpeedPanel.UpdateSpeedButtonDown;
end;

procedure TFormMain.UpdateSpeedButtonEnabled;
begin
  if SpeedPanel <> nil then
    SpeedPanel.UpdateSpeedButtonEnabled;
end;

end.
