unit fAbout;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellApi,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.jpeg;

type
  TFormAbout = class(TForm)
    ImageMoons: TImage;
    Panel1: TPanel;
    stTitle: TStaticText;
    ImageGLS: TImage;
    MemoDevelopers: TMemo;
    procedure ImageGLSClick(Sender: TObject);
    procedure ImageMoonsDblClick(Sender: TObject);
  private
  public
  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.dfm}

procedure TFormAbout.ImageGLSClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    'https://github.com/glscene', '', '', SW_SHOW);
end;

procedure TFormAbout.ImageMoonsDblClick(Sender: TObject);
begin
  MemoDevelopers.Visible := not MemoDevelopers.Visible;
end;

end.
