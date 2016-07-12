unit HNM.Raytrace.Geometry;

interface //#################################################################### ■

uses LUX, LUX.D1, LUX.D2, LUX.D3, LUX.Matrix.L4,
     LUX.Raytrace, LUX.Raytrace.Geometry,
     HNM.Raytrace;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyGeometry

     TMySphere = class( TRayImplicit )
     private
       ///// メソッド
       function GetLocalAABB : TSingleArea3D; override;
     protected
       _Radius :Single;
       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Radius :Single read _Radius write _Radius;
     end;

     TMySuperEllipse = class( TRayImplicit )
     private
       ///// メソッド
       function GetLocalAABB : TSingleArea3D; override;
     protected
       _E :Single;
       _N :Single;

       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
       function Power( P_:TdSingle ; E_:Single) : TdSingle;
       function Abs(P_:TdSingle):TdSingle;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property E :Single read _E write _E;
       property N :Single read _N write _N;
     end;

     TMyTorus = class( TRayImplicit )
     private
       ///// メソッド
       function GetLocalAABB : TSingleArea3D; override;
     protected
       _LargeR :Single;
       _SmallR :Single;

       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property LargeR :Single read _LargeR write _LargeR;
       property SmallR :Single read _SmallR write _SmallR;
     end;

     TMyBox = class( TRayImplicit )
     private
       ///// メソッド
       function GetLocalAABB : TSingleArea3D; override;
     protected
       _X :Single;
       _Y :Single;
       _Z :Single;
       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property X :Single read _X write _X;
       property Y :Single read _Y write _Y;
       property Z :Single read _Z write _Z;
     end;

     TMyCylinder = class( TRayImplicit )
     private
       ///// メソッド
       function GetLocalAABB : TSingleArea3D; override;
     protected
       _URadius :Single;
       _DRadius :Single;
       _Height : Single;

       ///// メソッド
       function Abs(P_:TdSingle):TdSingle;
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property URadius :Single read _URadius write _URadius;
       property DRadius :Single read _DRadius write _DRadius;
       property Height :Single read _Height write _Height;
     end;
     TMyAnd = class(TRayImplicit)
     private
         function GetLocalAABB : TSingleArea3D; override;
     protected
         _ObjectA :TRayImplicit;
         _ObjectB :TRayImplicit;
         _MatA : TSingleM4;
         _MatB : TSingleM4;
         function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
         constructor Create; override;
         destructor Destroy; override;
         property MatA:TSingleM4 read _MatA write _MatA;
         property MatB:TSingleM4 read _MatB write _MatB;
         property ObjectA :TRayImplicit read _ObjectA write _ObjectA;
         property ObjectB :TRayImplicit read _ObjectB write _ObjectB;
     end;
//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils, System.Math;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMySphere

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMySphere.GetLocalAABB: TSingleArea3D;
begin
     Result := TSingleArea3D.Create( -_Radius, -_Radius, -_Radius,
                                        +_Radius, +_Radius, +_Radius );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected


/////////////////////////////////////////////////////////////////////// メソッド

function TMySphere.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
begin
     Result := P_.Size - _Radius;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMySphere.Create;
begin
     inherited;

     Radius := 1;
end;

destructor TMySphere.Destroy;
begin

     inherited;
end;



//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyTorus

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMyTorus.GetLocalAABB: TSingleArea3D;
begin
     Result := TSingleArea3D.Create( -_LargeR - _SmallR, -_LargeR - _SmallR, -_SmallR,
                                        +_LargeR + _SmallR, +_LargeR + _SmallR, +_SmallR );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected


/////////////////////////////////////////////////////////////////////// メソッド

function TMyTorus.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
begin
     Result := Roo2(Pow2(Roo2(Pow2(P_.X) + Pow2(P_.Y))-_LargeR) + Pow2(P_.Z)) - _SmallR;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyTorus.Create;
begin
     inherited;

     _LargeR := 0.4;
     _SmallR := 0.1;

end;

destructor TMyTorus.Destroy;
begin

     inherited;
end;


//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMySuperEllipse

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMySuperEllipse.GetLocalAABB: TSingleArea3D;
var
     Size : Single;
begin
     Size := 1.0;
     Result := TSingleArea3D.Create( -Size, -Size, -Size,
                                        +Size, +Size, +Size );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected


/////////////////////////////////////////////////////////////////////// メソッド

function TMySuperEllipse.Power(P_: TdSingle; E_: Single) : TdSingle;
begin
     Result.o := System.Math.Power(P_.o, E_);
     Result.d := E_ * System.Math.Power(P_.o, E_-1.0) * P_.d;
end;

function TMySuperEllipse.Abs(P_: TdSingle) : TdSingle;
begin
     if P_.o < 0 then
     begin
        Result.o := -P_.o;
        Result.d := -P_.d;
     end else
     begin
        Result.o := P_.o;
        Result.d := P_.d;
     end;

end;

function TMySuperEllipse.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
begin
     Result := (Power((Power(Power(Abs(P_.X), 2.0/_E) + Power(Abs(P_.Y), 2.0/_E), _E/ _N)
                  + Power(Abs(P_.Z), 2.0/_N)), _N/2.0) - 1)
                  / Power((Power(Power(1.0/Sqrt(3.0), 2.0/_E)*2.0 , _E/ _N)+Power(1.0/Sqrt(3.0), 2.0/_N)), _N / 2.0);

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMySuperEllipse.Create;
begin
     inherited;

     E := 1;
     N := 1;
end;

destructor TMySuperEllipse.Destroy;
begin

     inherited;
end;


//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyBox

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMyBox.GetLocalAABB: TSingleArea3D;
begin
     Result := TSingleArea3D.Create( -_X, -_Y, -_Z,
                                        +_X, +_Y, +_Z );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected


/////////////////////////////////////////////////////////////////////// メソッド

function TMyBox.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
begin
  Result := 0;
  if (P_.X.o > _X) then
  begin
        Result := Result + (P_.X-_X)*(P_.X-_X);
  end else if (P_.X.o < -_X) then
  begin
        Result := Result + (P_.X+_X)*(P_.X+_X);
  end;

  if (P_.Y.o > _Y) then
  begin
        Result := Result + (P_.Y-_Y)*(P_.Y-_Y);
  end else if (P_.Y.o < -_Y) then
  begin
        Result := Result + (P_.Y+_Y)*(P_.Y+_Y);
  end;

  if (P_.Z.o > _Z) then
  begin
        Result := Result + (P_.Z-_Z)*(P_.Z-_Z);
  end else if (P_.Z.o < -_Z) then
  begin
        Result := Result + (P_.Z+_Z)*(P_.Z+_Z);
  end;

  Result := Roo2(Result);

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyBox.Create;
begin
     inherited;

     _X := 1.0;
     _Y := 1.0;
     _Z := 1.0;
end;

destructor TMyBox.Destroy;
begin

     inherited;
end;



//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMySphere

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMyCylinder.GetLocalAABB: TSingleArea3D;
var
  MaxRadius : Single;
begin
     MaxRadius := Max(_URadius, _DRadius);
     Result := TSingleArea3D.Create( -MaxRadius, -MaxRadius, -_Height,
                                        +MaxRadius, +MaxRadius, +_Height );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected


/////////////////////////////////////////////////////////////////////// メソッド

function TMyCylinder.Abs(P_: TdSingle) : TdSingle;
begin
     if P_.o < 0 then
     begin
        Result.o := -P_.o;
        Result.d := -P_.d;
     end else
     begin
        Result.o := P_.o;
        Result.d := P_.d;
     end;

end;

function TMyCylinder.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
var
  R : TdSingle;
  A, B:TdSingle;
begin
    R := Roo2(P_.X * P_.X + P_.Y * P_.Y);
    if _URadius = _DRadius then
    begin
        A := R - _URadius;
    end else begin
    A :=  -((2*_Height)/(_URadius-_DRadius)*R - P_.Z - (_URadius+_DRadius)/(_URadius-_DRadius)*_Height)
         /(Roo2(Pow2((2*_Height)/(_URadius-_DRadius)) + 1));
    end;

    B :=  Abs(P_.Z) - _Height;

    if A.o > B.o then
    begin
      Result := A;
    end else
    begin
      Result := B;
    end;


end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyCylinder.Create;
begin
     inherited;

     URadius := 1;
     DRadius := 1;
     Height := 1;
end;

destructor TMyCylinder.Destroy;
begin

     inherited;
end;




//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyAnd

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMyAnd.GetLocalAABB: TSingleArea3D;
var
  ObjectA_AABB : TSingleArea3D;
  ObjectB_AABB : TSingleArea3D;
  MaxX, MinX, MaxY, MinY, MaxZ, MinZ : Single;
begin
     {ObjectA_AABB := ObjectA.LocalAABB;
     ObjectB_AABB := ObjectB.LocalAABB;
     if ObjectA_AABB.Max.X > ObjectB_AABB.Max.X then
     begin
       MaxX := ObjectA_AABB.Max.X;
     end else
     begin
       MaxX := ObjectB_AABB.Max.X;
     end;
     if ObjectA_AABB.Max.Y > ObjectB_AABB.Max.Y then
     begin
       MaxY := ObjectA_AABB.Max.Y;
     end else
     begin
       MaxY := ObjectB_AABB.Max.Y;
     end;
     if ObjectA_AABB.Max.Z > ObjectB_AABB.Max.Z then
     begin
       MaxZ := ObjectA_AABB.Max.X;
     end else
     begin
       MaxZ := ObjectB_AABB.Max.X;
     end;
     if ObjectA_AABB.Min.X < ObjectB_AABB.Min.X then
     begin
       MinX := ObjectA_AABB.Min.X;
     end else
     begin
       MinX := ObjectB_AABB.Min.X;
     end;
     if ObjectA_AABB.Min.Y < ObjectB_AABB.Min.Y then
     begin
       MinY := ObjectA_AABB.Min.Y;
     end else
     begin
       MinY := ObjectB_AABB.Min.Y;
     end;
     if ObjectA_AABB.Min.Z > ObjectB_AABB.Min.Z then
     begin
       MinZ := ObjectA_AABB.Min.Z;
     end else
     begin
       MinZ := ObjectB_AABB.Min.Z;
     end;

     Result := TSingleArea3D.Create( MinX, MinY, MinZ,
                                        MaxX,  MaxY,  MaxZ ); }
     Result := TSingleArea3D.Create( -100, -100, -100,
                                        100,  100,  100 );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected


/////////////////////////////////////////////////////////////////////// メソッド


function TMyAnd.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
var
  PA, PB : TdSingle3D;
  DisA, DisB : TdSingle;
begin
   PA.X := _MatA._11 * P_.X + _MatA.Inverse()._12 * P_.Y
    + _MatA._13 * P_.Z + _MatA._14;
   PA.Y := _MatA._21 * P_.X + _MatA._22 * P_.Y
    + _MatA._23 * P_.Z + _MatA._24;
   PA.Z := _MatA._31 * P_.X + _MatA._32 * P_.Y
    + _MatA._33 * P_.Z + _MatA._34;

   PB.X := _MatB._11 * P_.X + _MatB._12 * P_.Y
    + _MatB._13 * P_.Z + _MatB._14;
   PB.Y := _MatB._21 * P_.X + _MatB._22 * P_.Y
    + _MatB._23 * P_.Z + _MatB._24;
   PB.Z := _MatB._31 * P_.X + _MatB._32 * P_.Y
    + _MatB._33 * P_.Z + _MatB._34;
   DisA := ObjectA.pDistanceFunc(PA);
   DisB := ObjectB.pDistanceFunc(PB);
   if DisA > DisB then
   begin
       Result := DisA;
   end else
   begin
       Result := DisB;
   end;

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyAnd.Create;
begin
     inherited;

     ObjectA := nil;
     ObjectB := nil;
end;

destructor TMyAnd.Destroy;
begin

     inherited;
end;






//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■