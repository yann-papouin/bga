unit Resources;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, PngImageList;

type
  TResourcesForm = class(TForm)
    Images16x16: TPngImageList;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  ResourcesForm: TResourcesForm;

implementation

{$R *.dfm}

end.
