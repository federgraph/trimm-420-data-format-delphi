unit RggUnit4;

interface

uses
  RiggVar.RG.Data;

type
  TRigg = class // (TRiggFS)
  public
    Data: TRggData;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFederData(fd: TRggData);
    procedure LoadFromFederData(fd: TRggData);
  end;

implementation

{ TRigg }

constructor TRigg.Create;
begin
  Data := TRggData.Create;
end;

destructor TRigg.Destroy;
begin
  Data.Free;
  inherited;
end;

procedure TRigg.LoadFromFederData(fd: TRggData);
begin
  Data.Assign(fd);
end;

procedure TRigg.SaveToFederData(fd: TRggData);
begin
  fd.Assign(Data);
end;

end.
