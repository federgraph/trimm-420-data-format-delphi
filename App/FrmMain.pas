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

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  private
    FScale: single;
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
    procedure UpdateLog;
    procedure UpdateReport;
    procedure ShowCurrentReport;
    procedure UpdateBackgroundColor(AColor: TAlphaColor);
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
  public
    procedure ShowReport(const Value: TRggReport);
    function GetShowDataText: Boolean;
    function GetShowDiffText: Boolean;
    function GetShowTrimmText: Boolean;
    procedure SetShowDataText(const Value: Boolean);
    procedure SetShowDiffText(const Value: Boolean);
    procedure SetShowTrimmText(const Value: Boolean);
    property ShowTrimmText: Boolean read GetShowTrimmText write SetShowTrimmText;
    property ShowDiffText: Boolean read GetShowDiffText write SetShowDiffText;
    property ShowDataText: Boolean read GetShowDataText write SetShowDataText;
  private
    ComponentsCreated: Boolean;
    procedure CreateComponents;
    procedure SetupMemo(MM: TMemo);
    procedure SetupText(T: TText; fs: single = 16);
  private
    Raster: Integer;
    Margin: Integer;
    SpeedPanelHeight: Integer;
    SpeedPanel: TActionSpeedBar;
    procedure InitSpeedButtons;
    procedure UpdateSpeedButtonDown;
    procedure UpdateSpeedButtonEnabled;
  public
    procedure UpdateColorScheme;
    procedure LayoutComponents;
    procedure HandleAction(fa: Integer);
  public
    Rigg: TRigg;
    ReportManager: TRggReportManager;
    procedure SetIsUp(const Value: Boolean);
    function GetIsUp: Boolean;
    property IsUp: Boolean read GetIsUp write SetIsUp;
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

  FScale := 1.0;
{$ifdef MSWINDOWS}
  { On MACOS Screen.WorkAreaHeight is not scaled,
    so it would be wrong to div by scale.

    On Windows Screen.WorkAreaHeight is scaled and should be divved. }
  FScale := Handle.Scale;
{$endif}

  Application.OnException := ApplicationEventsException;

  FormMain := self;
  Caption := ApplicationTitleText;
  Top := 20;
  Width := 1600;
  Height := 800;
  Margin := 10;
  Raster := 70;

  { RSP-20787 when TFormPosition.ScreenCenter}
//  Self.Position := TFormPosition.ScreenCenter;

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
  ReportLabel.BringToFront;
  TrimmText.BringToFront;

  Main.IsUp := True;
  Main.Trimm := 1;
  Main.UpdateTrimm0;
  ShowTrimm;

  Fill.Kind := TBrushKind.Solid;

{$ifdef MACOS}
  { OnKeyUp does not work well on Mac, RSP-2766 }
  OnKeyUp := nil;
  { we will use OnKeyDown instead }
  OnKeyDown := FormKeyUp;
{$endif}

  Application.OnHint := HandleShowHint;
  InitSpeedButtons;
  UpdateSpeedButtonDown;
  UpdateSpeedButtonEnabled;
  UpdateColorScheme;
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
    if FormShown then
      SpeedPanel.UpdateLayout;
  end;
end;

procedure TFormMain.UpdateBackgroundColor(AColor: TAlphaColor);
begin
  Self.Fill.Color := AColor;
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
  if not MainVar.AppIsClosing and Assigned(Main) then
    result := Main.IsUp
  else
    result := False;
end;

procedure TFormMain.SetIsUp(const Value: Boolean);
begin
  Main.IsUp := Value;
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

function TFormMain.GetShowDataText: Boolean;
begin
  result := ReportMemo.Visible and (ReportManager.CurrentReport = TRggReport.rgDataText);
end;

function TFormMain.GetShowDiffText: Boolean;
begin
  result := ReportMemo.Visible and (ReportManager.CurrentReport = TRggReport.rgDiffText);
end;

function TFormMain.GetShowTrimmText: Boolean;
begin
  result := ReportMemo.Visible and (ReportManager.CurrentReport = TRggReport.rgTrimmText);
end;

procedure TFormMain.ShowReport(const Value: TRggReport);
begin
  ReportManager.CurrentReport := Value;
  UpdateReport;
end;

procedure TFormMain.SetShowDataText(const Value: Boolean);
begin
  if Value then
  begin
    ShowReport(TRggReport.rgDataText);
  end
  else
  begin
    ReportMemo.Visible := False;
  end;
end;

procedure TFormMain.SetShowDiffText(const Value: Boolean);
begin
  if Value then
  begin
    ShowReport(TRggReport.rgDiffText);
  end
  else
  begin
    ReportMemo.Visible := False;
  end;
end;

procedure TFormMain.SetShowTrimmText(const Value: Boolean);
begin
  if Value then
  begin
    ShowReport(TRggReport.rgTrimmText);
  end
  else
  begin
    ReportMemo.Visible := False;
  end;
end;

procedure TFormMain.SetupText(T: TText; fs: single);
begin
  T.Parent := Self;
  T.WordWrap := False;
  T.HorzTextAlign := TTextAlign.Leading;
  T.Font.Family := 'Consolas';
  T.Font.Size := fs;
  T.AutoSize := True;
  T.HitTest := False;
end;

procedure TFormMain.SetupMemo(MM: TMemo);
begin
  if MM = nil then
    Exit;

  MM.ControlType := TControlType.Styled;
  MM.StyledSettings := [];
  MM.ShowScrollBars := True;
  MM.TextSettings.Font.Family := 'Consolas';
  MM.TextSettings.Font.Size := 14;
  MM.TextSettings.FontColor := claBlue;
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
  ReportManager.ShowCurrentReport;
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
  SpeedPanel.DarkMode := False;
  SpeedPanel.BigMode := False;

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

  ComponentsCreated := True;
end;

procedure TFormMain.LayoutComponents;
var
  th: Integer;
begin
  th := 30;
  SpeedPanelHeight := SpeedPanel.PanelHeight;

  SpeedPanel.Align := TAlignLayout.Top;
  SpeedPanel.Height := SpeedPanelHeight;

  HintText.Position.X := 20;
  HintText.Position.Y := SpeedPanelHeight + Margin;
  HintText.Height := th;

  ReportLabel.Position.X := 400;
  ReportLabel.Position.Y := SpeedPanelHeight + Margin;
  ReportLabel.Height := th;

  TrimmText.Position.X := 700;
  TrimmText.Position.Y := SpeedPanelHeight + Margin;
  TrimmText.Height := th;

  LogMemo.Anchors := [];
  LogMemo.Position.X := Margin;
  LogMemo.Position.Y := SpeedPanel.Height + th + Margin;
  LogMemo.Height := ClientHeight - LogMemo.Position.Y - Margin;
  LogMemo.Width := 400;
  LogMemo.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akBottom];

  ReportMemo.Anchors := [];
  ReportMemo.Position.X := LogMemo.Position.X + LogMemo.Width + Margin;
  ReportMemo.Position.Y := LogMemo.Position.Y;
  ReportMemo.Height := ClientHeight - ReportMemo.Position.Y - Margin;
  ReportMemo.Width := ClientWidth - ReportMemo.Position.X - Margin;
  ReportMemo.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom];
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

procedure TFormMain.UpdateColorScheme;
begin
  if not ComponentsCreated then
    Exit;

  UpdateBackgroundColor(SpeedPanel.SpeedColorScheme.claBack);

  if ReportLabel <> nil then
  ReportLabel.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claReport;

  HintText.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claHintText;
  TrimmText.TextSettings.FontColor := SpeedPanel.SpeedColorScheme.claTrimmText;
end;

end.
