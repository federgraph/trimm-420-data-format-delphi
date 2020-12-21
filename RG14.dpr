program RG14;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmMain in 'App\FrmMain.pas' {FormMain},
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.App.Model in 'App\RiggVar.App.Model.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RiggVar.FB.ActionLong in 'FB\RiggVar.FB.ActionLong.pas',
  RiggVar.FB.ActionShort in 'FB\RiggVar.FB.ActionShort.pas',
  RiggVar.FB.Classes in 'FB\RiggVar.FB.Classes.pas',
  RiggVar.FB.Scheme in 'FB\RiggVar.FB.Scheme.pas',
  RiggVar.FB.SpeedBar in 'FB\RiggVar.FB.SpeedBar.pas',
  RiggVar.FB.SpeedColor in 'FB\RiggVar.FB.SpeedColor.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas',
  RiggVar.Util.AppUtils in 'Util\RiggVar.Util.AppUtils.pas',
  RiggVar.RG.Data in 'RG\RiggVar.RG.Data.pas',
  RiggVar.RG.Def in 'RG\RiggVar.RG.Def.pas',
  RiggVar.RG.Report in 'RG\RiggVar.RG.Report.pas',
  RiggVar.RG.Main in 'RG\RiggVar.RG.Main.pas',
  RiggVar.RG.Strings in 'RG\RiggVar.RG.Strings.pas',
  RiggVar.RG.Speed01 in 'RG\RiggVar.RG.Speed01.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
