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

     TMyMaterialMirror = class( TRayMaterial )
     private
     protected
       _SpecRatio :TSingleRGB;
     public
       constructor Create;
       ///// プロパティ
       property SpecRatio :TSingleRGB read _SpecRatio write _SpecRatio;
       ///// メソッド
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
constructor TMyMaterialMirror.Create;
begin
     inherited;

     _SpecRatio := TSingleRGB.Create( 1, 1, 1 );
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TMyMaterialMirror.Scatter( const WorldRay_:TRayRay; const WorldHit_:TRayHit ) :TSingleRGB;
var
   ReA :TRayRay;
begin
     with ReA do
     begin
          Emt     := @WorldHit_;
          Ord     := WorldRay_.Ord + 1;
          Ray.Pos := WorldHit_.Pos;
          Ray.Vec := Reflect( WorldRay_.Ray.Vec, WorldHit_.Nor );
          Len     := Single.PositiveInfinity;
          Hit     := nil;
     end;

     Result := _SpecRatio * World.Raytrace( ReA );
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■