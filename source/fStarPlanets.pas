unit fStarPlanets;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.ExtDlgs,
  Vcl.BaseImageCollection,
  Vcl.ImageCollection,
  Vcl.ImgList,

  GLS.FileJPEG,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Material,
  GLS.Cadencer,
  GLS.LensFlare,
  GLS.Scene,
  GLS.Objects,
  GLS.Coordinates,
  GLS.SkyDome,
  GLS.SceneViewer,
  GLS.Texture,
  GLS.RenderContextInfo,
  GLS.Color,
  GLS.State,
  GLS.Utils,
  GLS.Context,
  GLS.TextureFormat,
  GLSL.TextureShaders,
  GLS.BaseClasses,
  GLS.Atmosphere,
  GLS.GeomObjects,
  GLS.VectorFileObjects,
  GLS.File3DS,
  GLS.FileObj,
  GLS.SimpleNavigation,

  fProjection,
  fSolarSystem;


type
  TFormStarPlanets = class(TForm)
    Scene: TGLScene;
    SceneViewer: TGLSceneViewer;
    Camera: TGLCamera;
    Planet: TGLSphere;
    LightStar: TGLLightSource;
    DirectOpenGL: TGLDirectOpenGL;
    Cadencer: TGLCadencer;
    Timer1: TTimer;
    Moon: TGLSphere;
    dcStar: TGLDummyCube;
    dcMoon: TGLDummyCube;
    LensStar: TGLLensFlare;
    MatLib: TGLMaterialLibrary;
    TexCombiner: TGLTexCombineShader;
    CameraControler: TGLCamera;
    AstroSkyDome: TGLSkyDome;
    ConstelLines: TGLLines;
    ConstelBoundaries: TGLLines;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    Open1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    miOpenFile: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    miInnerCore: TMenuItem;
    PanelTree: TPanel;
    TreeView: TTreeView;
    Options1: TMenuItem;
    miClearTreeView: TMenuItem;
    miConstelLines: TMenuItem;
    miConstelBoundaries: TMenuItem;
    OpenDialog: TOpenDialog;
    Saveas1: TMenuItem;
    SaveDialog: TSaveDialog;
    Atmosphere: TGLAtmosphere;
    PlanetSkyDome: TGLEarthSkyDome;
    PlanetCore: TGLDisk;
    Planetoid: TGLFreeForm;
    PlanetRingUp: TGLDisk;
    miWikiHelp: TMenuItem;
    ImageList1: TImageList;
    ImageCollection1: TImageCollection;
    PlanetRingDn: TGLDisk;
    miShowHideTV: TMenuItem;
    miShowHidePlanet: TMenuItem;
    miGoogleEarth: TMenuItem;
    N3: TMenuItem;
    miPlanetSkyDome: TMenuItem;
    StatusBar1: TStatusBar;
    miSystem: TMenuItem;
    miSolarSys: TMenuItem;
    miKepler22: TMenuItem;
    N5: TMenuItem;
    Projection1: TMenuItem;
    NightLights1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure DirectOpenGLRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure Timer1Timer(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure SceneViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SceneViewerDblClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure SceneViewerBeforeRender(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure miConstelLinesClick(Sender: TObject);
    procedure miConstelBoundariesClick(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure miOpenFileClick(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure miClearTreeViewClick(Sender: TObject);
    procedure miInnerCoreClick(Sender: TObject);
    procedure miWikiHelpClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure miShowHideTVClick(Sender: TObject);
    procedure miShowHidePlanetClick(Sender: TObject);
    procedure miGoogleEarthClick(Sender: TObject);
    procedure miPlanetSkyDomeClick(Sender: TObject);
    procedure miSolarSysClick(Sender: TObject);
    procedure Projection1Click(Sender: TObject);
  public
    ConstelLinesAlpha: Single;
    ConstelBoundariesAlpha: Single;
    TimeMultiplier: Single;
    mx, my, dmx, dmy: Integer;
    HighResResourcesLoaded: Boolean;
    CameraTimeSteps: Single;
    Radius, invAtmosphereHeight: Single;
    eyePos, lightingVector: TGLVector;
    diskNormal, diskRight, diskUp: TGLVector;
  private
    DataDir, StarDir, CurrentStar: TFileName;
    CatalogName, FileName: TFileName;
    function LonLatToPos(Lon, Lat: Single): TAffineVector;
    procedure LoadConstelLines;
    procedure LoadConstelBoundaries;
    function AtmosphereColor(const rayStart, rayEnd: TGLVector): TGLColorVector;
    function ComputeColor(var rayDest: TGLVector; mayHitGround: Boolean): TGLColorVector;
    procedure LoadHighResTexture(LibMat: TGLLibMaterial; const FileName: string);
  end;

var
  FormStarPlanets: TFormStarPlanets;

const
  cOpacity: Single = 5;
  // unrealistic thick atmospheres look better :)
  cAtmosphereRadius: Single = 0.55;
  // use value slightly lower than actual radius, for antialiasing effect
  cPlanetRadius: Single = 0.495;
  cLowAtmColor: TGLColorVector = (X:1; Y:1; Z:1; W:1);
  cHighAtmColor: TGLColorVector = (X:0; Y:0; Z:1; W:1);
  cIntDivTable: array[2..20] of Single =
    (1 / 2, 1 / 3, 1 / 4, 1 / 5, 1 / 6, 1 / 7, 1 / 8, 1 / 9, 1 / 10,
    1 / 11, 1 / 12, 1 / 13, 1 / 14, 1 / 15, 1 / 16, 1 / 17, 1 / 18, 1 / 19, 1 / 20);

const
  Plane0: array [0 .. 3] of Double = (0, 0, -1, 0.0);
  Plane1: array [0 .. 3] of Double = (-1, 0, 0, 0.0);
  Plane2: array [0 .. 3] of Double = (0, -1, 0, 0.0);

//-----------------------------------------
implementation
//-----------------------------------------

{$R *.dfm}

uses
  fAbout,
// accurate movements left for later...
  uSolarSys;

//---------------------------------------------

procedure TFormStarPlanets.FormCreate(Sender: TObject);
begin
  DataDir := ExtractFilePath(ParamStr(0)) + 'data';
  SetCurrentDir(DataDir);
  StarDir := DataDir + '\star';

  CatalogName := DataDir + '\catalog\hipparcos_9.stars';
  if FileExists(CatalogName) then
  begin
    AstroSkyDome.Bands.Clear;
    AstroSkyDome.Stars.Clear;
    AstroSkyDome.Stars.LoadStarsFile(CatalogName);
    AstroSkyDome.StructureChanged;
  end;

  // change dir to sun directory
  if DirectoryExists('star\sun') then
        ChDir('star\sun');
  CurrentStar := DataDir + '\star\sun\';
  Planet.Material.Texture.Disabled := False;
  Planet.Material.Texture.Image.LoadFromFile('earth.jpg');

  Atmosphere.PlanetRadius := Planet.Radius;
  Atmosphere.AtmosphereRadius := Planet.Radius + 0.05;
  Atmosphere.MoveTo(dcStar);
  Atmosphere.Opacity := cOpacity;

  TreeView.Select(TreeView.Items[3]);  // goto to Earth
  miWikiHelp.Caption := TreeView.Selected.Text + ' in ' + 'Wikipedia...';

  TimeMultiplier := Power(1, 3); // faster - Power(2, 3);
end;

// LonLatToPos
//
function TFormStarPlanets.LonLatToPos(Lon, Lat: Single): TAffineVector;
var
  f: Single;
begin
  SinCosine(Lat * (PI / 180), Result.Y, f);
  SinCosine(Lon * (360 / 24 * PI / 180), f, Result.X, Result.Z);
end;

//--------------------- Menu Items ---------------------------

// Show Constellation Lines
//
procedure TFormStarPlanets.miConstelLinesClick(Sender: TObject);
begin
  miConstelLines.Checked := not miConstelLines.Checked;
  ConstelLinesAlpha := 0.5 - ConstelLinesAlpha;
  if miConstelLines.Checked then
    LoadConstelLines;
end;

// Show Constellation Boundaries
//
procedure TFormStarPlanets.miConstelBoundariesClick(Sender: TObject);
begin
  miConstelBoundaries.Checked := not miConstelBoundaries.Checked;
  ConstelBoundariesAlpha := 0.5 - ConstelBoundariesAlpha;
  if miConstelBoundaries.Checked then
    LoadConstelBoundaries;
end;

// Show Planet
//
procedure TFormStarPlanets.miShowHidePlanetClick(Sender: TObject);
begin
  miShowHidePlanet.Checked := not miShowHidePlanet.Checked;
  if miShowHidePlanet.Checked then
  begin
    miShowHidePlanet.Caption := 'Show Planet';
    Planet.Visible := False;
    Planetoid.Visible := False;
    DirectOpenGl.Visible := False;
  end
  else
  begin
    miShowHidePlanet.Caption := 'Hide Planet';
    Planet.Visible := True;
    Planetoid.Visible := True;
    DirectOpenGl.Visible := True;
  end;
end;

// Show Inner Core
//
procedure TFormStarPlanets.miInnerCoreClick(Sender: TObject);
begin
  miInnerCore.Checked := not miInnerCore.Checked;
  if miInnerCore.Checked then
  begin
    FileName := CurrentStar + TreeView.Selected.Text;
    if FileExists(FileName + 'core.jpg') then
      PlanetCore.Material.Texture.Image.LoadFromFile(FileName + 'core.jpg')
    else
      PlanetCore.Material.Texture.Image.LoadFromFile(FileName + '.jpg');
    Planet.Stop := 180;
  end
  else
  begin
    Planet.Stop := 360;
  end;
end;

// Show Planet SkyDome
//
procedure TFormStarPlanets.miPlanetSkyDomeClick(Sender: TObject);
begin
  miPlanetSkyDome.Checked := not miPlanetSkyDome.Checked;
  if miPlanetSkyDome.Checked then
  begin
    PlanetSkyDome.Visible := True;
    AstroSkyDome.Visible := False;
  end
  else
  begin
    PlanetSkyDome.Visible := False;
    AstroSkyDome.Visible := True;
  end;
end;

// ShowHide TreeView
//
procedure TFormStarPlanets.miShowHideTVClick(Sender: TObject);
begin
  miShowHideTV.Checked := not miShowHideTV.Checked;
  if miShowHideTV.Checked then
  begin
    miShowHideTV.Caption := 'Show TreeView';
    PanelTree.Visible := False;
  end
  else
  begin
    miShowHideTV.Caption := 'Hide TreeView';
    PanelTree.Visible := True;
  end;
end;

// TreeViewClick
//
procedure TFormStarPlanets.TreeViewClick(Sender: TObject);
begin
  FileName := CurrentStar + TreeView.Selected.Text;

  if TreeView.Selected.StateIndex = -1 then    // Planet as TGLSphere
  begin
//  Planet.LoadFromFile(FileName + '.3ds'); // Sphere.3ds for TGLFreeForms
    Planet.Material.Texture.Image.LoadFromFile(FileName + '.jpg');
    Planet.Visible := True;
    Planetoid.Visible := False;
  end
  else    // StateIndex = 1 for Planetoid as TGLFreeForm
  begin
    Planetoid.LoadFromFile(FileName + '.3ds');
    Planetoid.Scale.X := 0.003; Planetoid.Scale.Y := 0.003; Planetoid.Scale.Z := 0.003;
    Planet.Visible := False;
    Planetoid.Visible := True;
    Planetoid.Material.Texture.Disabled := False;
    Planetoid.Material.Texture.Image.LoadFromFile(FileName + '.jpg');
  end;

  // Cores of Planets
  if miInnerCore.Checked then
  begin
    if FileExists(FileName  + 'core.jpg') then
      PlanetCore.Material.Texture.Image.LoadFromFile(FileName  + 'core.jpg')
    else
      PlanetCore.Material.Texture.Image.LoadFromFile(FileName  + '.jpg');
  end;

  // Rings
  if TreeView.Selected.Text = 'Saturn' then
  begin
    PlanetRingUp.Material.Texture.Image.LoadFromFile(FileName  + 'ring.jpg');
    PlanetRingDn.Material.Texture.Image.LoadFromFile(FileName  + 'ring.jpg');
    PlanetRingUp.Visible := True; PlanetRingDn.Visible := True;
  end
  else
  begin
    PlanetRingUp.Visible := False;
    PlanetRingDn.Visible := False;
  end;

  miWikiHelp.Caption := TreeView.Selected.Text + ' in ' + 'Wikipedia...';

  // Atmospheres
  if (TreeView.Selected.Text = 'Earth') then
    DirectOpenGL.Visible := True
  else
    DirectOpenGL.Visible := False;
end;

// SceneViewerBeforeRender
//
procedure TFormStarPlanets.SceneViewerBeforeRender(Sender: TObject);
begin
  LensStar.PreRender(Sender as TGLSceneBuffer);
  // if no multitexturing or no combiner support, turn off city lights
  MatLib.Materials[0].Shader := TexCombiner;
  MatLib.Materials[0].Texture2Name := 'earthNight';
end;

// Atmosphere Color
//
function TFormStarPlanets.AtmosphereColor(const rayStart, rayEnd: TGLVector)
  : TGLColorVector;
var
  i, n: Integer;
  atmPoint, normal: TGLVector;
  altColor: TGLColorVector;
  alt, rayLength, contrib, decay, intensity, invN: Single;

begin
  Result := clrTransparent;
  rayLength := VectorDistance(rayStart, rayEnd);
  n := Round(3 * rayLength * invAtmosphereHeight) + 2;
  if (n > 10) then
    n := 10;
  invN := cIntDivTable[n]; // 1/n;
  contrib := rayLength * invN * cOpacity;
  decay := 1 - contrib * 0.5;
  contrib := contrib * (1 / 1.1);
  for i := n - 1 downto 0 do
  begin
    VectorLerp(rayStart, rayEnd, i * invN, atmPoint);
    // diffuse lighting normal
    normal := VectorNormalize(atmPoint);
    // diffuse lighting intensity
    intensity := VectorDotProduct(normal, lightingVector) + 0.1;
    if (PInteger(@intensity)^ > 0) then
    begin
      // sample on the lit side
      intensity := intensity * contrib;
      alt := (VectorLength(atmPoint) - cPlanetRadius) * invAtmosphereHeight;
      VectorLerp(cLowAtmColor, cHighAtmColor, alt, altColor);
      Result.X := Result.X * decay + altColor.X * intensity;
      Result.Y := Result.Y * decay + altColor.Y * intensity;
      Result.Z := Result.Z * decay + altColor.Z * intensity;
    end
    else
    begin
      // sample on the dark side
      Result.X := Result.X * decay;
      Result.Y := Result.Y * decay;
      Result.Z := Result.Z * decay;
    end;
  end;
  Result.W := n * contrib * cOpacity * 0.1;
end;

// Compute Color
//
function TFormStarPlanets.ComputeColor(var rayDest: TGLVector; mayHitGround: Boolean): TGLColorVector;
var
  ai1, ai2, pi1, pi2: TGLVector;
  rayVector: TGLVector;
begin
  rayVector := VectorNormalize(VectorSubtract(rayDest, eyePos));
  if (RayCastSphereIntersect(eyePos, rayVector, NullHmgPoint, cAtmosphereRadius,
      ai1, ai2) > 1) then
  begin
    // atmosphere hit
    if mayHitGround and
      (RayCastSphereIntersect(eyePos, rayVector, NullHmgPoint, cPlanetRadius,
       pi1, pi2) > 0) then
    begin
      // hit ground
      Result := AtmosphereColor(ai1, pi1);
    end
    else
    begin
      // through atmosphere only
      Result := AtmosphereColor(ai1, ai2);
    end;
    rayDest := ai1;
  end
  else
    Result := clrTransparent;
end;

// Atmosphere with DirectOpenGLRender
//
procedure TFormStarPlanets.DirectOpenGLRender(Sender: TObject; var rci: TGLRenderContextInfo);
const
  cSlices = 60;
var
  i, j, k0, k1: Integer;
  cosCache, sinCache: array[0..cSlices] of Single;
  pVertex, pColor: PVectorArray;
begin
  eyepos := Camera.AbsolutePosition;

  diskNormal := VectorNegate(eyePos);
  NormalizeVector(diskNormal);
  diskRight := VectorCrossProduct(Camera.AbsoluteUp, diskNormal);
  NormalizeVector(diskRight);
  diskUp := VectorCrossProduct(diskNormal, diskRight);
  NormalizeVector(diskUp);

  invAtmosphereHeight := 1 / (cAtmosphereRadius - cPlanetRadius);
  lightingVector := VectorNormalize(LightStar.AbsolutePosition); // Star at infinity
  PrepareSinCosCache(sinCache, cosCache, 0, 360);

  GetMem(pVertex, 2 * (cSlices + 1) * SizeOf(TGLVector));
  GetMem(pColor, 2 * (cSlices + 1) * SizeOf(TGLVector));

  rci.GLStates.DepthWriteMask := False;
  rci.GLStates.Disable(stLighting);
  rci.GLStates.Enable(stBlend);
  rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
  for i := 0 to 13 do
  begin
    if (i < 5) then
      radius := cPlanetRadius * Sqrt(i * (1 / 5))
    else
      radius := cPlanetRadius + (i - 5.1) * (cAtmosphereRadius - cPlanetRadius) * (1 / 6.9);
    radius := SphereVisibleRadius(VectorLength(eyePos), radius);
    k0 := (i and 1) * (cSlices + 1);
    k1 := (cSlices + 1) - k0;
    for j := 0 to cSlices do
    begin
      VectorCombine(diskRight, diskUp, cosCache[j] * radius, sinCache[j] * radius,
        pVertex[k0 + j]);
      if (i < 13) then
        pColor[k0 + j] := ComputeColor(pVertex[k0 + j], i <= 7);
      if (i = 0) then
        Break;
    end;
    if (i > 1) then
    begin
      if (i = 13) then
      begin
        glBegin(GL_QUAD_STRIP);
        for j := cSlices downto 0 do
        begin
          glColor4fv(@pColor[k1 + j]);
          glVertex3fv(@pVertex[k1 + j]);
          glColor4fv(@clrTransparent);
          glVertex3fv(@pVertex[k0 + j]);
        end;
        glEnd;
      end
      else
      begin
        glBegin(GL_QUAD_STRIP);
        for j := cSlices downto 0 do
        begin
          glColor4fv(@pColor[k1 + j]);
          glVertex3fv(@pVertex[k1 + j]);
          glColor4fv(@pColor[k0 + j]);
          glVertex3fv(@pVertex[k0 + j]);
        end;
        glEnd;
      end;
    end
    else if (i = 1) then
    begin
      glBegin(GL_TRIANGLE_FAN);
      glColor4fv(@pColor[k1]);
      glVertex3fv(@pVertex[k1]);
      for j := k0 + cSlices downto k0 do
      begin
        glColor4fv(@pColor[j]);
        glVertex3fv(@pVertex[j]);
      end;
      glEnd;
    end;
  end;
  rci.GLStates.DepthWriteMask := True;
  FreeMem(pVertex);
  FreeMem(pColor);
end;

//----------------------------------------------
// Constellation Lines
//----------------------------------------------
procedure TFormStarPlanets.LoadConstelLines;
var
  sl, line: TStrings;
  pos1, pos2: TAffineVector;
  i: Integer;
begin
  sl := TStringList.Create;
  line := TStringList.Create;
  sl.LoadFromFile(DataDir + '\constellation\ConstLines.dat');
  for i := 0 to sl.Count - 1 do
  begin
    line.CommaText := sl[i];
    pos1 := LonLatToPos(StrToFloatDef(line[0], 0), StrToFloatDef(line[1], 0));
    ConstelLines.AddNode(pos1);
    pos2 := LonLatToPos(StrToFloatDef(line[2], 0), StrToFloatDef(line[3], 0));
    ConstelLines.AddNode(pos2);
  end;
  sl.Free;
  line.Free;
end;

//-----------------------------------------------
// Constellation Boundaries
//-----------------------------------------------
procedure TFormStarPlanets.LoadConstelBoundaries;
var
  sl, line: TStrings;
  pos1, pos2: TAffineVector;
  i: Integer;
begin
  sl := TStringList.Create;
  line := TStringList.Create;
  sl.LoadFromFile(DataDir + '\constellation\ConstBoundaries.dat');
//  sl.LoadFromFile(DataDir + 'Constellations\and.dat');  // only Andromeda
  for i := 0 to sl.Count - 1 do
  begin
    line.CommaText := sl[i];
    pos1 := LonLatToPos(StrToFloatDef(line[0], 0), StrToFloatDef(line[1], 0));
    ConstelBoundaries.AddNode(pos1);
    pos2 := LonLatToPos(StrToFloatDef(line[2], 0), StrToFloatDef(line[3], 0));
    ConstelBoundaries.AddNode(pos2);
  end;
  sl.Free;
  line.Free;
end;

// CadencerProgress
//
procedure TFormStarPlanets.CadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  d : Double;
  p : TAffineVector;
begin
  d := GMTDateTimeToJulianDay(Now - 2 + newTime * TimeMultiplier);

  // make rotate
  Planet.TurnAngle := Planet.TurnAngle + deltaTime * TimeMultiplier;
  Planetoid.TurnAngle := Planetoid.TurnAngle + deltaTime * TimeMultiplier;
  p := ComputePlanetPosition(cSunOrbitalElements, d);
  ScaleVector(p, 0.5 * cAUToKilometers * (1 / cEarthRadius));
 /// LSSun.Position.AsAffineVector := p;      //stop sun motion

  // make moon rotate on itself and around earth (not sure about the rotation direction!)
  p := ComputePlanetPosition(cMoonOrbitalElements, d);
  ScaleVector(p, 0.5 * cAUToKilometers * (1 / cEarthRadius));
  dcMoon.TurnAngle := dcMoon.TurnAngle + deltaTime * timeMultiplier / 29.5;
  Moon.TurnAngle := 180 - dcMoon.TurnAngle;

  // honour camera movements
  if (dmy <> 0) or (dmx <> 0) then
  begin
    CameraControler.MoveAroundTarget(ClampValue(dmy * 0.3, -5, 5),
      ClampValue(dmx * 0.3, -5, 5));
    dmx := 0;
    dmy := 0;
  end;
  // this gives us smoother camera movements
  cameraTimeSteps := cameraTimeSteps + deltaTime;
  while cameraTimeSteps > 0.005 do
  begin
    Camera.Position.AsVector := VectorLerp(Camera.Position.AsVector,
      CameraControler.Position.AsVector, 0.05);
    cameraTimeSteps := cameraTimeSteps - 0.005;
  end;
  // smooth constellation lines appearance/disappearance
  if ConstelLines.LineColor.Alpha <> ConstelLinesAlpha then
  begin
    ConstelLines.LineColor.Alpha :=
      ClampValue(ConstelLines.LineColor.Alpha + Sign(ConstelLinesAlpha -
                 ConstelLines.LineColor.Alpha) * deltaTime, 0, 0.5);
    ConstelLines.Visible := (ConstelLines.LineColor.Alpha > 0);
  end;
  // smooth constellation boundaries appearance/disappearance
  if ConstelBoundaries.LineColor.Alpha <> ConstelBoundariesAlpha then
  begin
    ConstelBoundaries.LineColor.Alpha :=
      ClampValue(ConstelBoundaries.LineColor.Alpha + Sign(ConstelBoundariesAlpha -
                 ConstelBoundaries.LineColor.Alpha) * deltaTime, 0, 0.5);
    ConstelBoundaries.Visible := (ConstelBoundaries.LineColor.Alpha > 0);
  end;
end;

// SceneViewerMouseDown
//
procedure TFormStarPlanets.SceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := x;
  my := y;
end;

// SceneViewerMouseMove
//
procedure TFormStarPlanets.SceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin

  if Shift = [ssLeft] then
  begin
    dmx := dmx + (mx - x);
    dmy := dmy + (my - y);
  end
  else if Shift = [ssRight] then
    Camera.FocalLength := Camera.FocalLength * Power(1.05, (my - y) * 0.1);
  mx := x;
  my := y;

end;

// FormMouseWheel
//
procedure TFormStarPlanets.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  f: Single;
begin

  if (WheelDelta > 0) or (CameraControler.Position.VectorLength > 0.90) then
  begin
    f := Power(1.05, WheelDelta * (1 / 120));
    CameraControler.AdjustDistanceToTarget(f);
  end;
  Handled := True;

end;

// SceneViewerDblClick
//
procedure TFormStarPlanets.SceneViewerDblClick(Sender: TObject);
begin
  SceneViewer.OnMouseMove := nil;
  if WindowState = wsMaximized then
  begin
    WindowState := wsNormal;
    PanelTree.Visible := True;
  end
  else
  begin
    WindowState := wsMaximized;
    PanelTree.Visible := False;
  end;
  SceneViewer.OnMouseMove := SceneViewerMouseMove;
end;

// LoadHighResTexture
//
procedure TFormStarPlanets.LoadHighResTexture(LibMat: TGLLibMaterial; const FileName: string);
begin
  if FileExists(FileName) then
  begin
    LibMat.Material.Texture.Compression := tcStandard;
    LibMat.Material.Texture.Image.LoadFromFile(fileName);
  end;
end;


// FormKeyPress
//
procedure TFormStarPlanets.FormKeyPress(Sender: TObject; var Key: Char);

begin
  case Key of
    'e', 'E':    // Planet
      begin
        Camera.MoveTo(dcStar);
        CameraControler.MoveTo(dcStar);
        Camera.TargetObject := dcStar;
        CameraControler.TargetObject := dcStar;
      end;
(*
    'h':
      if not highResResourcesLoaded then
      begin
        SceneViewer.Cursor := crHourGlass;
        try
          if DirectoryExists(CurrentStar) then
          begin
            LoadHighResTexture(MatLib.Materials[0], 'land_ocean_ice_4096.jpg');
            LoadHighResTexture(MatLib.Materials[1], 'land_ocean_ice_lights_4096.jpg');
            LoadHighResTexture(MatLib.Materials[2], 'moon_2048.jpg');
          end;
          SceneViewer.Buffer.AntiAliasing := aa2x;
        finally
          SceneViewer.Cursor := crDefault;
        end;
        highResResourcesLoaded := True;
      end;
*)
    '0'..'9': timeMultiplier := Power(Integer(Key) - Integer('0'), 3);
    #27: Close;
  end;
end;

//  Timer1Timer
//
procedure TFormStarPlanets.Timer1Timer(Sender: TObject);
begin
  Caption := Format('Stellar Planets ' + '%.1f FPS', [SceneViewer.FramesPerSecond]);
  SceneViewer.ResetPerformanceMonitor;
end;

//----------------------------------------------------------------

procedure TFormStarPlanets.miSolarSysClick(Sender: TObject);
begin
  with TFormSolarSys.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

//----------------------------------------------------------------

procedure TFormStarPlanets.About1Click(Sender: TObject);
begin
  with TFormAbout.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

// miClear
//
procedure TFormStarPlanets.miClearTreeViewClick(Sender: TObject);
begin
  TreeView.Items.Clear;
end;

procedure TFormStarPlanets.miWikiHelpClick(Sender: TObject);
var
  S: String;
begin
  if (TreeView.Selected.Level = 0)   then
    // Planets or Asteroids, sometimes with S + '_(planet)' like for ../Mercury_(planet)
    S :=  'https://en.wikipedia.org/wiki/' + TreeView.Selected.Text
  else
    // Moons
    S :=  'https://en.wikipedia.org/wiki/' + TreeView.Selected.Text + '_(moon)';
  ShellExecute(0, 'open', PWideChar(S), '', '', SW_SHOW);
end;


procedure TFormStarPlanets.Projection1Click(Sender: TObject);
begin
  with TFormProjection.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

//  miOpenFile
//
procedure TFormStarPlanets.miOpenFileClick(Sender: TObject);
begin
  OpenDialog.Filter := 'Star Planets (*.star)|*.star';
  OpenDialog.InitialDir := StarDir;
  OpenDialog.DefaultExt := '*.star';
  if OpenDialog.Execute then
  begin
    TreeView.LoadFromFile(OpenDialog.FileName);
    CurrentStar := ExtractFilePath(OpenDialog.FileName);
    TreeView.Select(TreeView.Items[0]);  // goto to new Star
    TreeViewClick(Sender);
  end;
end;

// miSaveAs
//
procedure TFormStarPlanets.SaveAs1Click(Sender: TObject);
begin
  SaveDialog.Filter := 'Star Planets (*.star)|*.star';
  SaveDialog.InitialDir := StarDir;
  SaveDialog.DefaultExt := '*.star';
  if SaveDialog.Execute then
  begin
    TreeView.SaveToFile(SaveDialog.FileName);
    CurrentStar := GetCurrentDir();
  end;
end;

// miGoogleEarth
//
procedure TFormStarPlanets.miGoogleEarthClick(Sender: TObject);
var
  S: String;
begin
  S := 'https://earth.google.com/';
  ShellExecute(0, 'open', PWideChar(S), '', '', SW_SHOW);
end;

//------------------------------------------------------------

procedure TFormStarPlanets.Exit1Click(Sender: TObject);
begin
  Close;
end;

initialization
  FormatSettings.DecimalSeparator := '.';
finalization
  FormatSettings.DecimalSeparator := ',';

end.
