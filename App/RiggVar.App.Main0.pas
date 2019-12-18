unit RiggVar.App.Main0;

interface

uses
  System.SysUtils,
  System.Classes,
  RiggVar.Util.Logger;

type
  TMain0 = class
  protected
    FL: TStringList;
    procedure CopyText;
  public
    Logger: TLogger;
    IsRetina: Boolean;
    RetinaScale: single;
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure DropTargetDropped(fn: string); virtual;
  end;

implementation

uses
  FMX.Platform,
  RiggVar.App.Main;

{ TMain0 }

constructor TMain0.Create;
begin
//  Main := self;
  inherited;
  Logger := TLogger.Create;
  FL := TStringList.Create;
end;

destructor TMain0.Destroy;
begin
  Logger.Free;
  FL.Free;
  inherited;
end;

procedure TMain0.DropTargetDropped(fn: string);
begin

end;

procedure TMain0.Init;
begin
//  ...

//  Main.RggMain.Init;

//  Frame3D.InitOK := True;
//  IsUp := True;
end;

procedure TMain0.CopyText;
var
  cbs: IFMXClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(cbs)) then
  begin
    cbs.SetClipboard(FL.Text);
    Logger.Info('in CopyText ( check clipboard )');
  end;
end;

end.

