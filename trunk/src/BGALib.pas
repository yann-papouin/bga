unit BGALib;

interface

uses
  SysUtils;

type

  TBgaGetFileByPath = function(Sender: TObject; const VirtualPath : string) : string of object;

  procedure EnableBFDecimal;
  procedure DisableBFDecimal;

implementation

const
  BATTLEFIELD_DECIMAL = '.';

var
  SystemDecimalSeparator : Char;

  procedure EnableBFDecimal;
  begin
    if SystemDecimalSeparator = #0 then
    begin
      SystemDecimalSeparator := DecimalSeparator;
      DecimalSeparator := BATTLEFIELD_DECIMAL;
    end;
  end;

  procedure DisableBFDecimal;
  begin
    if SystemDecimalSeparator <> #0 then
    begin
      DecimalSeparator := SystemDecimalSeparator;
      SystemDecimalSeparator := #0;
    end;
  end;
end.
