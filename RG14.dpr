program RG14;

uses
  System.StartUpCopy,
  FMX.Forms,
  FrmMain in 'App\FrmMain.pas' {FormMain},
  RiggVar.App.Main in 'App\RiggVar.App.Main.pas',
  RiggVar.App.Main0 in 'App\RiggVar.App.Main0.pas',
  RiggVar.App.Main1 in 'App\RiggVar.App.Main1.pas',
  RggUnit4 in 'Core\RggUnit4.pas',
  RiggVar.FB.ActionConst in 'FB\RiggVar.FB.ActionConst.pas',
  RiggVar.FB.Classes in 'FB\RiggVar.FB.Classes.pas',
  RiggVar.Util.Logger in 'Util\RiggVar.Util.Logger.pas',
  RiggVar.Util.AppUtils in 'Util\RiggVar.Util.AppUtils.pas',
  RiggVar.RG.Data in 'RG\RiggVar.RG.Data.pas',
  RiggVar.RG.Def in 'RG\RiggVar.RG.Def.pas',
  RiggVar.RG.Report in 'RG\RiggVar.RG.Report.pas',
  RiggVar.RG.Main in 'RG\RiggVar.RG.Main.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
