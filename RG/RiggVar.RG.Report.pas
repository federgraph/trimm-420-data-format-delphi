unit RiggVar.RG.Report;

interface

uses
  System.SysUtils,
  System.Classes;

type
  TRggReport = (
    rgLog,
    rgJson,
    rgData,
    rgShort,
    rgLong,

    rgTrimmText,
    rgJsonText,
    rgDataText,
    rgDiffText,

    rgAusgabeRL,
    rgAusgabeRP,
    rgAusgabeRLE,
    rgAusgabeRPE,
    rgAusgabeDiffL,
    rgAusgabeDiffP,

    rgXML,
    rgDebugReport,
    rgReadme,
    rgNone
  );

  TRggReportManager = class
  private
    ML: TStrings; // not owned
    FCurrentReport: TRggReport;
    procedure SetCurrentReport(const Value: TRggReport);
  public
    Counter: Integer;
    constructor Create(MemoLines: TStrings);
    procedure ShowCurrentReport;
    function GetReportCaption(r: TRggReport): string;
    property CurrentReport: TRggReport read FCurrentReport write SetCurrentReport;
  end;

implementation

uses
  RiggVar.App.Main,
  RiggVar.RG.Data;

{ TRggReportManager }

constructor TRggReportManager.Create(MemoLines: TStrings);
begin
  ML := MemoLines;
end;

procedure TRggReportManager.SetCurrentReport(const Value: TRggReport);
begin
  FCurrentReport := Value;
end;

function TRggReportManager.GetReportCaption(r: TRggReport): string;
begin
  Inc(Counter);
  case r of
    rgLog: result := 'Log';
    rgJson: result := 'Rigg.Data.WriteJson';
    rgData: result := 'Rigg.Data.WriteReport';
    rgShort: result := 'Trimm-Item Short';
    rgLong: result := 'Trimm-Item Long';
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

procedure TRggReportManager.ShowCurrentReport;
var
  cr: TRggData;
begin
  cr := Main.Rigg.Data;
  ML.BeginUpdate;
  try
    ML.Clear;
    case CurrentReport of
      rgLog: ML.Text := Main.Logger.TL.Text;
      rgJson: cr.WriteJSon(ML);
      rgData: cr.WriteReport(ML);
      rgShort:
      begin
        cr.WantAll := False;
        cr.SaveTrimmItem(ML);
      end;
      rgLong:
      begin
        cr.WantAll := True;
        cr.SaveTrimmItem(ML);
      end;
      rgDebugReport:
      begin
        Main.DoCleanReport;
        ML.Text := Main.Logger.TL.Text;
      end;
    end;
  finally
    ML.EndUpdate;
  end;
end;

end.
