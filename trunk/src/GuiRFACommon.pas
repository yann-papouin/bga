unit GuiRFACommon;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees;

type
  TRFACommonForm = class(TForm)
    RFAList: TVirtualStringTree;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  RFACommonForm: TRFACommonForm;

implementation

{$R *.dfm}

end.
