unit RiggVar.RG.Report;

interface

uses
  System.SysUtils;

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
    FCounter: Integer;
    FCurrentReport: TRggReport;
    FXmlAllTags: Boolean;
    procedure SetCurrentReport(const Value: TRggReport);
    procedure SetXmlAllTags(const Value: Boolean);
  public
    function GetReportCaption(r: TRggReport): string;
    property CurrentReport: TRggReport read FCurrentReport write SetCurrentReport;
    property XmlAllTags: Boolean read FXmlAllTags write SetXmlAllTags;
  end;

implementation

{ TRggReportManager }

procedure TRggReportManager.SetCurrentReport(const Value: TRggReport);
begin
  FCurrentReport := Value;
end;

procedure TRggReportManager.SetXmlAllTags(const Value: Boolean);
begin
  FXmlAllTags := Value;
end;

function TRggReportManager.GetReportCaption(r: TRggReport): string;
begin
  Inc(FCounter);
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
  result := Format('%s (%d)', [result, FCounter]);
end;

end.
