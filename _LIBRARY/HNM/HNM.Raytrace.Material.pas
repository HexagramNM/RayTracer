unit HNM.Raytrace.Material;

interface //#################################################################### ■

uses LUX, LUX.D3, LUX.Matrix.L4, LUX.Color,
     LUX.Raytrace, LUX.Raytrace.Material,
     HNM.Raytrace;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterial

     TMyMaterial = class( TRayMaterial )
     private
     protected
       _DiffRatio :TSingleRGB;
     public
       constructor Create;
       ///// プロパティ
       property DiffRatio :TSingleRGB read _DiffRatio write _DiffRatio;
       ///// メソッド
       function Scatter( const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB; override;
     end;

     TMyMaterialFresnelMirror = class( TRayMaterial )
     private
     protected
       _Fresnel0Ratio :TSingleRGB;
     public
       constructor Create;
       ///// プロパティ
       property Fresnel0Ratio :TSingleRGB read _Fresnel0Ratio write _Fresnel0Ratio;
       ///// メソッド
       function Fresnel(Fresnel0:TSingleRGB ;Nor, Ray:TSingle3D):TSingleRGB;
       function Scatter( const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB; override;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■
uses System.SysUtils, System.Math,
     LUX.D2, LUX.Geometry.D3;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterial

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyMaterial.Create;
begin
     inherited;

     _DiffRatio := TSingleRGB.Create( 1, 1, 1 );
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TMyMaterial.Scatter( const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB;
var
   I :Integer;
   L :TRayLight;
   A :TRayRay;
   H :TRayHit;
   D :Single;
begin
     Result := 0;

     for I := 0 to World.LightsN-1 do
     begin
          L := World.Lights[ I ];

          with A do
          begin
               Emt     := @WorldHit_;
               Ord     := WorldRay_.Ord + 1;
               Ray.Pos := WorldHit_.Pos;
             //Ray.Vec
             //Len
               Hit     := @H;
          end;

          with H do
          begin
               Ray := @A;
               Obj := nil;
             //Nor
             //Tan
             //Bin
             //Tex
          end;

          if L.RayJoins( A, H ) then
          begin
               D := DotProduct( WorldHit_.Nor, A.Ray.Vec );

               if D < 0 then D := 0;

               Result := Result + D * L.Color * _DiffRatio;
          end;
     end;
end;


//TMyMaterialMirror
constructor TMyMaterialFresnelMirror.Create;
begin
     inherited;

     _Fresnel0Ratio := TSingleRGB.Create( 1, 1, 1 );
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TMyMaterialFresnelMirror.Fresnel(Fresnel0:TSingleRGB ;Nor, Ray:TSingle3D):TSingleRGB;
begin
    if DotProduct(Ray, Nor) >= 0 then
    begin
        Result := Fresnel0 + (TSingleRGB.Create(1.0, 1.0, 1.0) - Fresnel0) *
              Power(1.0-DotProduct(Ray, Nor), 5.0);
    end else
    begin
        Result := TSingleRGB.Create(1.0, 1.0, 1.0);
    end;
end;

function TMyMaterialFresnelMirror.Scatter( const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB;
var
   ReA :TRayRay;
begin
     with ReA do
     begin
          Emt     := @WorldHit_;
          Ord     := WorldRay_.Ord + 1;
          Ray.Pos := WorldHit_.Pos;
          Ray.Vec := Reflect( WorldRay_.Ray.Vec, WorldHit_.Nor );
          Ray.Vec.X := Ray.Vec.X + 0.05*(Random-0.5);
          Ray.Vec.Y := Ray.Vec.Y + 0.05*(Random-0.5);
          Ray.Vec.Z := Ray.Vec.Z + 0.05*(Random-0.5);
          Ray.Vec := Ray.Vec.Unitor;
          Len     := Single.PositiveInfinity;
          Hit     := nil;
     end;

     Result := Fresnel(_Fresnel0Ratio, WorldHit_.Nor.Unitor, -WorldRay_.Ray.Unitor.Vec) * World.Raytrace( ReA );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■