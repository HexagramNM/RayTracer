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
       function abs(P_:TdSingle):TdSingle;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property E :Single read _E write _E;
       property N :Single read _N write _N;
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


//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■