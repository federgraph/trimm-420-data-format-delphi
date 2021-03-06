﻿unit RiggVar.RG.Speed01;

interface

uses
  RiggVar.FB.SpeedBar,
  RiggVar.FB.SpeedColor,
  System.UIConsts,
  System.Classes,
  FMX.Types,
  FMX.StdCtrls;

type
  TActionSpeedBarRG01 = class(TActionSpeedBar)
  private
    LogBtn: TSpeedButton;
    ShortBtn: TSpeedButton;
    LongBtn: TSpeedButton;
    StateBtn: TSpeedButton;
    JsonBtn: TSpeedButton;

    MT0Btn: TSpeedButton;
    ReadTrimmFileBtn: TSpeedButton;
    SaveTrimmFileBtn: TSpeedButton;
    CopyTrimmFileBtn: TSpeedButton;
    CopyTrimmItemBtn: TSpeedButton;
    PasteTrimmItemBtn: TSpeedButton;
    CopyAndPasteBtn: TSpeedButton;

    M1Btn: TSpeedButton;
    M10Btn: TSpeedButton;
    P1Btn: TSpeedButton;
    P10Btn: TSpeedButton;

    SandboxedBtn: TSpeedButton;

    ToggleDarkModeBtn: TSpeedButton;
    ToggleButtonSizeBtn: TSpeedButton;

    T1Btn: TSpeedButton;
    T2Btn: TSpeedButton;
    T3Btn: TSpeedButton;
    T4Btn: TSpeedButton;
    T5Btn: TSpeedButton;
    T6Btn: TSpeedButton;
    T7Btn: TSpeedButton;
    T8Btn: TSpeedButton;

    procedure M10BtnClick(Sender: TObject);
    procedure M1BtnClick(Sender: TObject);
    procedure P1BtnClick(Sender: TObject);
    procedure P10BtnClick(Sender: TObject);

    procedure SandboxedBtnClick(Sender: TObject);

    procedure MT0BtnClick(Sender: TObject);

    procedure CopyAndPasteBtnClick(Sender: TObject);
    procedure CopyTrimmFileBtnClick(Sender: TObject);
    procedure CopyTrimmItemBtnClick(Sender: TObject);

    procedure PasteTrimmItemBtnClick(Sender: TObject);
    procedure ReadTrimmFileBtnClick(Sender: TObject);
    procedure SaveTrimmFileBtnClick(Sender: TObject);

    procedure LogBtnClick(Sender: TObject);
    procedure ShortBtnClick(Sender: TObject);
    procedure LongBtnClick(Sender: TObject);
    procedure StateBtnClick(Sender: TObject);
    procedure JsonBtnClick(Sender: TObject);

    procedure TrimmBtnClick(Sender: TObject);
  public
    procedure InitSpeedButtons; override;
  end;

implementation

uses
  FrmMain,
  RiggVar.App.Main,
  RiggVar.FB.ActionConst,
  RiggVar.RG.Report;

{ TActionSpeedBarRG01 }

procedure TActionSpeedBarRG01.InitSpeedButtons;
var
  sb: TSpeedBtn;
begin
  { Special Buttons }

  BtnColorValue := clvData;

  sb := AddSpeedBtn('ToggleButtonSizeBtn', BtnGroupSpace);
  ToggleButtonSizeBtn := sb;
  sb.Text := 'BS';
  sb.Hint := 'Toggle Button Size';
  sb.StaysPressed := True;
  sb.IsPressed := False;
  sb.Tag := faToggleButtonSize;
  sb.OnClick := ToggleButtonSizeBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ToggleDarkModeBtn');
  ToggleDarkModeBtn := sb;
  sb.Text := 'DM';
  sb.Hint := 'Toggle Dark Mode';
  sb.StaysPressed := True;
  sb.IsPressed := False;
  sb.Tag := faToggleDarkMode;
  sb.OnClick := ToggleDarkModeBtnClick;
  InitSpeedButton(sb);

  { Log Button }

  BtnColorValue := clvLog;

  sb := AddSpeedBtn('LogBtn', BtnGroupSpace);
  LogBtn := sb;
  sb.Text := 'Log';
  sb.Hint := 'Log Btn';
  sb.OnClick := LogBtnClick;
  InitSpeedButton(sb);

  { Report Buttons }

  BtnColorValue := clvReport;

  sb := AddSpeedBtn('ShortBtn', BtnGroupSpace);
  ShortBtn := sb;
  sb.Text := 'dat';
  sb.Hint := 'Data (Short)';
  sb.OnClick := ShortBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('LongBtn');
  LongBtn := sb;
  sb.Text := 'DAT';
  sb.Hint := 'Data (Long)';
  sb.OnClick := LongBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('StateBtn');
  StateBtn := sb;
  sb.Text := 'Sta';
  sb.Hint := 'State Btn';
  sb.OnClick := StateBtnClick;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('JsonBtn');
  JsonBtn := sb;
  sb.Text := 'Jsn';
  sb.Hint := 'Json Btn';
  sb.OnClick := JsonBtnClick;
  InitSpeedButton(sb);

  { Check Box Buttons }

  BtnColorValue := clvOption;

  sb := AddSpeedBtn('SandboxedBtn', BtnGroupSpace);
  SandboxedBtn := sb;
  sb.Text := 'SB';
  sb.Hint := 'Sandboxed';
  sb.StaysPressed := True;
  sb.IsPressed := False;
  sb.OnClick := SandboxedBtnClick;
  sb.Tag := faToggleSandboxed;
  InitSpeedButton(sb);

  { Data Buttons }

  BtnColorValue := clvData;

  sb := AddSpeedBtn('MT0Btn', BtnGroupSpace);
  MT0Btn := sb;
  sb.Text := 'MT0';
  sb.Hint := 'Update Trimm 0';
  sb.OnClick := MT0BtnClick;
  sb.Tag := faUpdateTrimm0;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('ReadTrimmFileBtn');
  ReadTrimmFileBtn := sb;
  sb.Text := 'rtf';
  sb.Hint := 'Read Trimm File';
  sb.OnClick := ReadTrimmFileBtnClick;
  sb.Tag := faReadTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('SaveTrimmFileBtn');
  SaveTrimmFileBtn := sb;
  sb.Text := 'stf';
  sb.Hint := 'Save Trimm File';
  sb.OnClick := SaveTrimmFileBtnClick;
  sb.Tag := faSaveTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyTrimmFileBtn');
  CopyTrimmFileBtn := sb;
  sb.Text := 'ctf';
  sb.Hint := 'Copy Trimm File';
  sb.OnClick := CopyTrimmFileBtnClick;
  sb.Tag := faCopyTrimmFile;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyTrimmItemBtn');
  CopyTrimmItemBtn := sb;
  sb.Text := 'cti';
  sb.Hint := 'Copy Trimm Item';
  sb.OnClick := CopyTrimmItemBtnClick;
  sb.Tag := faCopyTrimmItem;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('PasteTrimmItemBtn');
  PasteTrimmItemBtn := sb;
  sb.Text := 'pti';
  sb.Hint := 'Paste Trimm Item';
  sb.OnClick := PasteTrimmItemBtnClick;
  sb.Tag := faPasteTrimmItem;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('CopyAndPasteBtn');
  CopyAndPasteBtn := sb;
  sb.Text := 'M';
  sb.Hint := 'Copy And Paste';
  sb.OnClick := CopyAndPasteBtnClick;
  sb.Tag := faCopyAndPaste;
  InitSpeedButton(sb);

  { Param Value Buttons }

  BtnColorValue := clvWheel;

  sb := AddSpeedBtn('M10Btn', BtnGroupSpace);
  M10Btn := sb;
  sb.Text := '-10';
  sb.Hint := 'Param Value Minus 10';
  sb.OnClick := M10BtnClick;
  sb.Tag := faParamValueMinus10;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('M1Btn');
  M1Btn := sb;
  sb.Text := '-1';
  sb.Hint := 'Param Value Minus 1';
  sb.OnClick := M1BtnClick;
  sb.Tag := faParamValueMinus1;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('P1Btn');
  P1Btn := sb;
  sb.Text := '+1';
  sb.Hint := 'Param Value Plus 1';
  sb.OnClick := P1BtnClick;
  sb.Tag := faParamValuePlus1;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('P10Btn');
  P10Btn := sb;
  sb.Text := '+10';
  sb.Hint := 'Param Value Plus 10';
  sb.OnClick := P10BtnClick;
  sb.Tag := faParamValuePlus10;
  InitSpeedButton(sb);

  { Trimm Buttons }

  BtnColorValue := clvTrimm;

  sb := AddSpeedBtn('T1Btn', BtnGroupSpace);
  T1Btn := sb;
  sb.Text := 'T1';
  sb.Hint := 'Trimm 1 Btn';
  sb.OnClick := TrimmBtnClick;
  sb.Tag := faTrimm1;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T2Btn');
  T2Btn := sb;
  sb.Text := 'T2';
  sb.Hint := 'Trimm 2 Btn';
  sb.OnClick := TrimmBtnClick;
  sb.Tag := faTrimm2;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T3Btn');
  T3Btn := sb;
  sb.Text := 'T3';
  sb.Hint := 'Trimm 3 Btn';
  sb.OnClick := TrimmBtnClick;
  sb.Tag := faTrimm3;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T4Btn');
  T4Btn := sb;
  sb.Text := 'T4';
  sb.Hint := 'Trimm 4 Btn';
  sb.OnClick := TrimmBtnClick;
  sb.Tag := faTrimm4;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T5Btn');
  T5Btn := sb;
  sb.Text := 'T5';
  sb.Hint := 'Trimm 5 Btn';
  sb.OnClick := TrimmBtnClick;
  sb.Tag := faTrimm5;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T6Btn');
  T6Btn := sb;
  sb.Text := 'T6';
  sb.Hint := 'Trimm 6 Btn';
  sb.OnClick := TrimmBtnClick;
  sb.Tag := faTrimm6;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T7Btn');
  T7Btn := sb;
  sb.Text := 'T7';
  sb.Hint := 'Trimm 7 Btn (420)';
  sb.OnClick := TrimmBtnClick;
  sb.Tag := fa420;
  InitSpeedButton(sb);

  sb := AddSpeedBtn('T8Btn');
  T8Btn := sb;
  sb.Text := 'T8';
  sb.Hint := 'Trimm 8 Btn (Logo)';
  sb.OnClick := TrimmBtnClick;
  sb.Tag := faLogo;
  sb.SpecialWidth := 70;
  InitSpeedButton(sb);
end;

procedure TActionSpeedBarRG01.CopyTrimmItemBtnClick(Sender: TObject);
begin
  Main.CopyTrimmItem;
  FormMain.ShowTrimm;
end;

procedure TActionSpeedBarRG01.PasteTrimmItemBtnClick(Sender: TObject);
begin
  Main.PasteTrimmItem;
  FormMain.ShowTrimm;
end;

procedure TActionSpeedBarRG01.CopyAndPasteBtnClick(Sender: TObject);
begin
  Main.CopyAndPaste;
  FormMain.ShowTrimm;
end;

procedure TActionSpeedBarRG01.CopyTrimmFileBtnClick(Sender: TObject);
begin
  Main.CopyTrimmFile;
  FormMain.ShowTrimm;
end;

procedure TActionSpeedBarRG01.ReadTrimmFileBtnClick(Sender: TObject);
begin
  Main.ReadTrimmFile;
  FormMain.ShowTrimm;
end;

procedure TActionSpeedBarRG01.SaveTrimmFileBtnClick(Sender: TObject);
begin
  Main.SaveTrimmFile;
  FormMain.ShowTrimm;
end;

procedure TActionSpeedBarRG01.MT0BtnClick(Sender: TObject);
begin
  Main.UpdateTrimm0;
  FormMain.ShowTrimm;
end;

procedure TActionSpeedBarRG01.SandboxedBtnClick(Sender: TObject);
begin
  MainVar.IsSandboxed := not MainVar.IsSandboxed;
end;

procedure TActionSpeedBarRG01.M10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus10);
  FormMain.ShowTrimm;
end;

procedure TActionSpeedBarRG01.M1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValueMinus1);
  FormMain.ShowTrimm;
end;

procedure TActionSpeedBarRG01.P10BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus10);
  FormMain.ShowTrimm;
end;

procedure TActionSpeedBarRG01.P1BtnClick(Sender: TObject);
begin
  Main.HandleAction(faParamValuePlus1);
  FormMain.ShowTrimm;
end;

procedure TActionSpeedBarRG01.LogBtnClick(Sender: TObject);
begin
  Main.DoCleanReport;
  FormMain.UpdateLog;
end;

procedure TActionSpeedBarRG01.ShortBtnClick(Sender: TObject);
begin
  FormMain.ReportManager.CurrentReport := rgShort;
  FormMain.ShowCurrentReport;
  FormMain.ReportLabel.Text := 'Short Data';
end;

procedure TActionSpeedBarRG01.LongBtnClick(Sender: TObject);
begin
  FormMain.ReportManager.CurrentReport := rgLong;
  FormMain.ShowCurrentReport;
  FormMain.ReportLabel.Text := 'Long Data';
end;

procedure TActionSpeedBarRG01.StateBtnClick(Sender: TObject);
begin
  FormMain.ReportManager.CurrentReport := rgData;
  FormMain.ShowCurrentReport;
end;

procedure TActionSpeedBarRG01.JsonBtnClick(Sender: TObject);
begin
  FormMain.ReportManager.CurrentReport := rgJson;
  FormMain.ShowCurrentReport;
end;

procedure TActionSpeedBarRG01.TrimmBtnClick(Sender: TObject);
var
  fa: Integer;
begin
  fa := (Sender as TComponent).Tag;
  Main.HandleAction(fa);
  FormMain.ShowTrimm;
end;

end.
