unit FrmMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.UIConsts,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TFormMain = class(TForm)
    T1Btn: TButton;
    T2Btn: TButton;
    T3Btn: TButton;
    T4Btn: TButton;
    T5Btn: TButton;
    T6Btn: TButton;
    T7Btn: TButton;
    T8Btn: TButton;
    PasteTrimmItemBtn: TButton;
    CopyTrimmFileBtn: TButton;
    ReadTrimmFileBtn: TButton;
    SaveTrimmFileBtn: TButton;
    MT0Btn: TButton;
    CopyTrimmItemBtn: TButton;
    LogMemo: TMemo;
    CopyAndPasteBtn: TButton;
    ReportBtn: TButton;
    LogBtn: TButton;
    StateMemo: TMemo;
    StateBtn: TButton;
    JsonBtn: TButton;
    cbSandboxed: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ReportBtnClick(Sender: TObject);
    procedure T1BtnClick(Sender: TObject);
    procedure LogBtnClick(Sender: TObject);
    procedure CopyTrimmItemBtnClick(Sender: TObject);
    procedure PasteTrimmItemBtnClick(Sender: TObject);
    procedure CopyTrimmFileBtnClick(Sender: TObject);
    procedure ReadTrimmFileBtnClick(Sender: TObject);
    procedure SaveTrimmFileBtnClick(Sender: TObject);
    procedure StateBtnClick(Sender: TObject);
    procedure MT0BtnClick(Sender: TObject);
    procedure CopyAndPasteBtnClick(Sender: TObject);
    procedure JsonBtnClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure cbSandboxedChange(Sender: TObject);
  private
    procedure InitRetina;
    procedure SetupMemo(Memo: TMemo);
    procedure UpdateLog;
    procedure UpdateMemo;
    procedure UpdateReport;
    procedure UpdateJson;
  public
    ResizeCounter: Integer;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    function GetOpenFileName(dn, fn: string): string;
    function GetSaveFileName(dn, fn: string): string;
  end;

var
  FormMain: TFormMain;

implementation

uses
  RiggVar.App.Main,
  RiggVar.FB.Classes;

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Main := TMain.Create;
  Main.Logger.Verbose := True;

  InitRetina;

  T1Btn.Tag := 1;
  T2Btn.Tag := 2;
  T3Btn.Tag := 3;
  T4Btn.Tag := 4;
  T5Btn.Tag := 5;
  T6Btn.Tag := 6;
  T7Btn.Tag := 7;
  T8Btn.Tag := 8;

  T2Btn.OnClick := T1Btn.OnClick;
  T3Btn.OnClick := T1Btn.OnClick;
  T4Btn.OnClick := T1Btn.OnClick;
  T5Btn.OnClick := T1Btn.OnClick;
  T6Btn.OnClick := T1Btn.OnClick;
  T7Btn.OnClick := T1Btn.OnClick;
  T8Btn.OnClick := T1Btn.OnClick;

  SetupMemo(LogMemo);
  SetupMemo(StateMemo);

  LogMemo.TextSettings.FontColor := claBlue;

  UpdateLog;
  UpdateMemo;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Main.Free;
  Main := nil;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  Inc(ResizeCounter);
end;

procedure TFormMain.SetupMemo(Memo: TMemo);
begin
  //Memo.Align := TAlignLayout.Client;
  Memo.ControlType := TControlType.Styled;
  Memo.StyledSettings := [];
  Memo.ShowScrollBars := True;
  Memo.TextSettings.Font.Family := 'Consolas';
  Memo.TextSettings.Font.Size := 14;
  Memo.TextSettings.FontColor := claBlack;
end;

procedure TFormMain.InitRetina;
var
  t: single;
begin
  t := self.Handle.Scale; //Viewport1.Scene.GetSceneScale;
  if t > 1 then
  begin
    if Main <> nil then
    begin
      Main.IsRetina := True;
    end;
  end;
  if Main <> nil then
  begin
     Main.Logger.InfoVerbose('in TFormMain.InitRetina');
     Main.Logger.InfoVerbose('  Scale = ' + FloatToStr(t));
     Main.Logger.InfoVerbose('  Retina = ' + BoolStr[Main.IsRetina]);
  end;
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

procedure TFormMain.UpdateLog;
begin
  LogMemo.Lines.Text := Main.Logger.TL.Text;
end;

procedure TFormMain.UpdateReport;
begin
  StateMemo.Lines.Clear;
  Main.CurrentTrimm.WriteReport(StateMemo.Lines);
end;

procedure TFormMain.UpdateJson;
begin
  StateMemo.Lines.Clear;
  Main.CurrentTrimm.WriteJson(StateMemo.Lines);
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

procedure TFormMain.MT0BtnClick(Sender: TObject);
begin
  Main.UpdateTrimm0;
  UpdateLog;
end;

procedure TFormMain.StateBtnClick(Sender: TObject);
begin
  //StateMemo.Lines.Text := Main.TrimmData;
  UpdateReport;
end;

procedure TFormMain.JsonBtnClick(Sender: TObject);
begin
  UpdateJson;
end;

procedure TFormMain.cbSandboxedChange(Sender: TObject);
begin
  IsSandboxed := cbSandboxed.IsChecked;
end;

end.
