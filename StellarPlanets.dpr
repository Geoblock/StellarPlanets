(*
   StarPlanetsDs - Stars with exoplanets
   See accompanying Readme.txt for user instructions.
   The atmospheric effect is rendered in DirectOpenGLRender, which essentially
   renders a disk, with color of the vertices computed via ray-tracing. Not that
   the tesselation of the disk has been hand-optimized so as to reduce CPU use
   while retaining quality.
   Catalog of stars is built into the TGLSkyDome, but constellations are rendered
   via a TGLLines, which is filled in the LoadConstellationLines method.
*)
program StellarPlanets;

uses
  Forms,
  fStarPlanets in 'Source\fStarPlanets.pas' {FormStarPlanets},
  uSolarSys in 'Source\Code\uSolarSys.pas',
  fAbout in 'Source\fAbout.pas' {FormAbout},
  fParams in 'Source\fParams.pas' {FrameParams: TFrame},
  fSolarSystem in 'Source\fSolarSystem.pas' {FormSolarSys},
  fProjection in 'Source\fProjection.pas' {FormProjection};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormStarPlanets, FormStarPlanets);
  Application.Run;
end.
