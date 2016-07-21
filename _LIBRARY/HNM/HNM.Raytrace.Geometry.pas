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

     TMyTetrahedron = class(TRayImplicit)
     private
       ///// メソッド
       NorVec : array[1..4] of TSingle3D;
       function GetLocalAABB : TSingleArea3D; override;
     protected
       _Size :Single;

       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Size :Single read _Size write _Size;
     end;

     TMyOctahedron = class(TRayImplicit)
     private
       ///// メソッド
       NorVec : array[1..8] of TSingle3D;
       function GetLocalAABB : TSingleArea3D; override;
     protected
       _Size :Single;

       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property Size :Single read _Size write _Size;
     end;

     TMyDodecahedron = class(TRayImplicit)
     private
       ///// メソッド
       D : Single;
       NorVec : array[1..12] of TSingle3D;
       function GetLocalAABB : TSingleArea3D; override;
     protected
       _InternalRadius :Single;

       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property InternalRadius :Single read _InternalRadius write _InternalRadius;
     end;

     TMyIcosahedron = class(TRayImplicit)
     private
       ///// メソッド
       D : Single;
       TmpVec : array[1..12] of TSingle3D;
       NorVec : array[1..20] of TSingle3D;
       function GetLocalAABB : TSingleArea3D; override;
     protected
       _InternalRadius :Single;

       ///// メソッド
       function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property InternalRadius :Single read _InternalRadius write _InternalRadius;
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
         function RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean; override;
         constructor Create; override;
         destructor Destroy; override;
         property MatA:TSingleM4 read _MatA write _MatA;
         property MatB:TSingleM4 read _MatB write _MatB;
         property ObjectA :TRayImplicit read _ObjectA write _ObjectA;
         property ObjectB :TRayImplicit read _ObjectB write _ObjectB;
     end;
     TMyOr = class(TRayImplicit)
     private
         function GetLocalAABB : TSingleArea3D; override;
     protected
         _ObjectA :TRayImplicit;
         _ObjectB :TRayImplicit;
         _MatA : TSingleM4;
         _MatB : TSingleM4;
         function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
         function RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean; override;
         constructor Create; override;
         destructor Destroy; override;
         property MatA:TSingleM4 read _MatA write _MatA;
         property MatB:TSingleM4 read _MatB write _MatB;
         property ObjectA :TRayImplicit read _ObjectA write _ObjectA;
         property ObjectB :TRayImplicit read _ObjectB write _ObjectB;
     end;

     TMySub = class(TRayImplicit)
     private
         function GetLocalAABB : TSingleArea3D; override;
     protected
         _ObjectA :TRayImplicit;
         _ObjectB :TRayImplicit;
         _MatA : TSingleM4;
         _MatB : TSingleM4;
         function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
         function RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean; override;
         constructor Create; override;
         destructor Destroy; override;
         property MatA:TSingleM4 read _MatA write _MatA;
         property MatB:TSingleM4 read _MatB write _MatB;
         property ObjectA :TRayImplicit read _ObjectA write _ObjectA;
         property ObjectB :TRayImplicit read _ObjectB write _ObjectB;
     end;

     TMyChamfer = class(TRayImplicit)
     private
         function GetLocalAABB : TSingleArea3D; override;
     protected
         _ObjectA :TRayImplicit;
         _R : Single;
         function DistanceFunc( const P_:TdSingle3D ) :TdSingle; override;
     public
         function RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean; override;
         constructor Create; override;
         destructor Destroy; override;
         property ObjectA :TRayImplicit read _ObjectA write _ObjectA;
         property R :Single read _R write _R;
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
var
  Cof :TdSingle;
begin
     Cof := Power((Power(Power(1.0{/Sqrt(3.0)}, 2.0/_E)*2.0 , _E/ _N)+Power(1.0{/Sqrt(3.0)}, 2.0/_N)), _N / 2.0);

     Result := (Power((Power(Power(Abs(P_.X), 2.0/_E) + Power(Abs(P_.Y), 2.0/_E), _E/ _N)
                  + Power(Abs(P_.Z), 2.0/_N)), _N/2.0) - 1)
                  / Cof;

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



//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyCylinder

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


//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyTetrahedron

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMyTetrahedron.GetLocalAABB: TSingleArea3D;
begin
     Result := TSingleArea3D.Create( -Size/2.0, -Size/2.0, -_Size/2.0,
                                        +Size/2.0, +Size/2.0, +Size/2.0 );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected


/////////////////////////////////////////////////////////////////////// メソッド

function TMyTetrahedron.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
var
  I : Integer;
  Tmp : TdSingle;
begin
  for I := 1 to 4 do
    begin
        Tmp := P_.X * NorVec[i].X + P_.Y * NorVec[i].Y  + P_.Z * NorVec[i].Z - Size * Sqrt(6.0)/12.0 ;
        if I=1 then
        begin
            Result := Tmp;
        end else if Result < Tmp then
        begin
            Result := Tmp;
        end;

    end;

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyTetrahedron.Create;
begin
     inherited;
      NorVec[1] := TSingle3D.Create(0, 1.0/3.0, 2*Sqrt(2.0)/3.0);
      NorVec[2] := TSingle3D.Create(Sqrt(6.0)/3.0, 1.0/3.0, -Sqrt(2.0)/3.0);
      NorVec[3] := TSingle3D.Create(-Sqrt(6.0)/3.0, 1.0/3.0, -Sqrt(2.0)/3.0);
      NorVec[4] := TSingle3D.Create(0.0, -1.0, 0.0);
     Size := 1;
end;

destructor TMyTetrahedron.Destroy;
begin

     inherited;
end;



//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyOctahedron

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMyOctahedron.GetLocalAABB: TSingleArea3D;
begin
     Result := TSingleArea3D.Create( -Size, -Size, -_Size,
                                        +Size, +Size, +Size );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected


/////////////////////////////////////////////////////////////////////// メソッド

function TMyOctahedron.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
var
  I : Integer;
  Tmp : TdSingle;
begin
  for I := 1 to 8 do
    begin
        Tmp := P_.X * NorVec[i].X + P_.Y * NorVec[i].Y  + P_.Z * NorVec[i].Z - Size * Sqrt(6.0)/6.0 ;
        if I=1 then
        begin
            Result := Tmp;
        end else if Result < Tmp then
        begin
            Result := Tmp;
        end;

    end;

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyOctahedron.Create;
begin
     inherited;
     NorVec[1] := TSingle3D.Create(Sqrt(6.0)/3.0, Sqrt(3.0)/ 3.0, 0.0);
     NorVec[2] := TSingle3D.Create(-Sqrt(6.0)/3.0, Sqrt(3.0)/ 3.0, 0.0);
     NorVec[3] := TSingle3D.Create(0.0, Sqrt(3.0)/ 3.0, Sqrt(6.0)/3.0);
     NorVec[4] := TSingle3D.Create(0.0, Sqrt(3.0)/ 3.0, -Sqrt(6.0)/3.0);
     NorVec[5] := TSingle3D.Create(Sqrt(6.0)/3.0, -Sqrt(3.0)/ 3.0, 0.0);
     NorVec[6] := TSingle3D.Create(-Sqrt(6.0)/3.0, -Sqrt(3.0)/ 3.0, 0.0);
     NorVec[7] := TSingle3D.Create(0.0, -Sqrt(3.0)/ 3.0, Sqrt(6.0)/3.0);
     NorVec[8] := TSingle3D.Create(0.0, -Sqrt(3.0)/ 3.0, -Sqrt(6.0)/3.0);
     Size := 1;
end;

destructor TMyOctahedron.Destroy;
begin

     inherited;
end;



//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyDodecahedron

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMyDodecahedron.GetLocalAABB: TSingleArea3D;
begin
     Result := TSingleArea3D.Create( -2*Internalradius, -2*Internalradius, -2*Internalradius,
                                        +2*Internalradius, +2*Internalradius, +2*Internalradius );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected


/////////////////////////////////////////////////////////////////////// メソッド

function TMyDodecahedron.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
var
  I : Integer;
  Tmp : TdSingle;
begin
  for I := 1 to 12 do
    begin
        Tmp := P_.X * NorVec[i].X + P_.Y * NorVec[i].Y  + P_.Z * NorVec[i].Z - Internalradius ;
        if I=1 then
        begin
            Result := Tmp;
        end else if Result < Tmp then
        begin
            Result := Tmp;
        end;

    end;

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyDodecahedron.Create;
begin
     inherited;

     InternalRadius := 1;
     D := Cos(72.0 / 180.0 * Pi)
     /(1.0 - Cos(72.0 / 180.0 * Pi));
     NorVec[1] := TSingle3D.Create(0.0, 1.0, 0.0);
     NorVec[2] := TSingle3D.Create(Sqrt(1.0 - D*D), D, 0.0);
     NorVec[3] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(72.0 * Pi/180.0), D, Sqrt(1.0 - D*D)*Sin(72.0 * Pi/180.0));
     NorVec[4] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(144.0 * Pi/180.0), D, Sqrt(1.0 - D*D)*Sin(144.0 * Pi/180.0));
     NorVec[5] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(216.0 * Pi/180.0), D, Sqrt(1.0 - D*D)*Sin(216.0 * Pi/180.0));
     NorVec[6] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(288.0 * Pi/180.0), D, Sqrt(1.0 - D*D)*Sin(288.0 * Pi/180.0));
     NorVec[7] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(36.0 * Pi/180.0), -D, Sqrt(1.0 - D*D)*Sin(36.0 * Pi/180.0));
     NorVec[8] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(108.0 * Pi/180.0), -D, Sqrt(1.0 - D*D)*Sin(108.0 * Pi/180.0));
     NorVec[9] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(180.0 * Pi/180.0), -D, Sqrt(1.0 - D*D)*Sin(180.0 * Pi/180.0));
     NorVec[10] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(252.0 * Pi/180.0), -D, Sqrt(1.0 - D*D)*Sin(252.0 * Pi/180.0));
     NorVec[11] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(324.0 * Pi/180.0), -D, Sqrt(1.0 - D*D)*Sin(324.0 * Pi/180.0));
     NorVec[12] := TSingle3D.Create(0.0, -1.0, 0.0);
end;

destructor TMyDodecahedron.Destroy;
begin

     inherited;
end;


//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyIcosahedron

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMyIcosahedron.GetLocalAABB: TSingleArea3D;
begin
     Result := TSingleArea3D.Create( -2*Internalradius, -2*Internalradius, -2*Internalradius,
                                        +2*Internalradius, +2*Internalradius, +2*Internalradius );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected


/////////////////////////////////////////////////////////////////////// メソッド

function TMyIcosahedron.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
var
  I : Integer;
  Tmp : TdSingle;
begin
  for I := 1 to 20 do
    begin
        Tmp := P_.X * NorVec[i].X + P_.Y * NorVec[i].Y  + P_.Z * NorVec[i].Z - Internalradius ;
        if I=1 then
        begin
            Result := Tmp;
        end else if Result < Tmp then
        begin
            Result := Tmp;
        end;

    end;

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyIcosahedron.Create;
var
     I:Integer;
begin
     inherited;

     InternalRadius := 1;
     D := Cos(72.0 / 180.0 * Pi)
     /(1.0 - Cos(72.0 / 180.0 * Pi));
     TmpVec[1] := TSingle3D.Create(0.0, 1.0, 0.0);
     TmpVec[2] := TSingle3D.Create(Sqrt(1.0 - D*D), D, 0.0);
     TmpVec[3] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(72.0 * Pi/180.0), D, Sqrt(1.0 - D*D)*Sin(72.0 * Pi/180.0));
     TmpVec[4] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(144.0 * Pi/180.0), D, Sqrt(1.0 - D*D)*Sin(144.0 * Pi/180.0));
     TmpVec[5] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(216.0 * Pi/180.0), D, Sqrt(1.0 - D*D)*Sin(216.0 * Pi/180.0));
     TmpVec[6] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(288.0 * Pi/180.0), D, Sqrt(1.0 - D*D)*Sin(288.0 * Pi/180.0));
     TmpVec[7] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(36.0 * Pi/180.0), -D, Sqrt(1.0 - D*D)*Sin(36.0 * Pi/180.0));
     TmpVec[8] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(108.0 * Pi/180.0), -D, Sqrt(1.0 - D*D)*Sin(108.0 * Pi/180.0));
     TmpVec[9] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(180.0 * Pi/180.0), -D, Sqrt(1.0 - D*D)*Sin(180.0 * Pi/180.0));
     TmpVec[10] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(252.0 * Pi/180.0), -D, Sqrt(1.0 - D*D)*Sin(252.0 * Pi/180.0));
     TmpVec[11] := TSingle3D.Create(Sqrt(1.0 - D*D)*Cos(324.0 * Pi/180.0), -D, Sqrt(1.0 - D*D)*Sin(324.0 * Pi/180.0));
     TmpVec[12] := TSingle3D.Create(0.0, -1.0, 0.0);
     NorVec[1] := (TmpVec[1] + TmpVec[2] + TmpVec[3]).Unitor;
     NorVec[2] := (TmpVec[1] + TmpVec[3] + TmpVec[4]).Unitor;
     NorVec[3] := (TmpVec[1] + TmpVec[4] + TmpVec[5]).Unitor;
     NorVec[4] := (TmpVec[1] + TmpVec[5] + TmpVec[6]).Unitor;
     NorVec[5] := (TmpVec[1] + TmpVec[6] + TmpVec[2]).Unitor;

     NorVec[6] := (TmpVec[7] + TmpVec[2] + TmpVec[3]).Unitor;
     NorVec[7] := (TmpVec[8] + TmpVec[3] + TmpVec[4]).Unitor;
     NorVec[8] := (TmpVec[9] + TmpVec[4] + TmpVec[5]).Unitor;
     NorVec[9] := (TmpVec[10] + TmpVec[5] + TmpVec[6]).Unitor;
     NorVec[10] := (TmpVec[11] + TmpVec[6] + TmpVec[2]).Unitor;

     NorVec[11] := (TmpVec[12] + TmpVec[7] + TmpVec[8]).Unitor;
     NorVec[12] := (TmpVec[12] + TmpVec[8] + TmpVec[9]).Unitor;
     NorVec[13] := (TmpVec[12] + TmpVec[9] + TmpVec[10]).Unitor;
     NorVec[14] := (TmpVec[12] + TmpVec[10] + TmpVec[11]).Unitor;
     NorVec[15] := (TmpVec[12] + TmpVec[11] + TmpVec[7]).Unitor;

     NorVec[16] := (TmpVec[3] + TmpVec[7] + TmpVec[8]).Unitor;
     NorVec[17] := (TmpVec[4] + TmpVec[8] + TmpVec[9]).Unitor;
     NorVec[18] := (TmpVec[5] + TmpVec[9] + TmpVec[10]).Unitor;
     NorVec[19] := (TmpVec[6] + TmpVec[10] + TmpVec[11]).Unitor;
     NorVec[20] := (TmpVec[2] + TmpVec[11] + TmpVec[7]).Unitor;
     {for I := 1 to 20 do
       begin
           NorVec[I] := NorVec[I].U
       end;}
end;

destructor TMyIcosahedron.Destroy;
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

function TMyAnd.RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
begin
     Result := RayCast( WorldRay_, WorldHit_ );

     //Result := RayCastChilds( WorldRay_, WorldHit_ ) or Result;
end;


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




//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyOr

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMyOr.GetLocalAABB: TSingleArea3D;
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


function TMyOr.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
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
       Result := DisB;
   end else
   begin
       Result := DisA;
   end;

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

function TMyOr.RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
begin
     Result := RayCast( WorldRay_, WorldHit_ );

     //Result := RayCastChilds( WorldRay_, WorldHit_ ) or Result;
end;


constructor TMyOr.Create;
begin
     inherited;

     ObjectA := nil;
     ObjectB := nil;
end;

destructor TMyOr.Destroy;
begin

     inherited;
end;



//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMySub

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMySub.GetLocalAABB: TSingleArea3D;
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


function TMySub.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
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
   if DisA > -DisB then
   begin
       Result := DisA;
   end else
   begin
       Result := -DisB;
   end;

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

function TMySub.RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
begin
     Result := RayCast( WorldRay_, WorldHit_ );

     //Result := RayCastChilds( WorldRay_, WorldHit_ ) or Result;
end;


constructor TMySub.Create;
begin
     inherited;

     ObjectA := nil;
     ObjectB := nil;
end;

destructor TMySub.Destroy;
begin

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyChamfer

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TMyChamfer.GetLocalAABB: TSingleArea3D;
begin

     Result := TSingleArea3D.Create( -100, -100, -100,
                                        100,  100,  100 );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected


/////////////////////////////////////////////////////////////////////// メソッド


function TMyChamfer.DistanceFunc( const P_:TdSingle3D ) :TdSingle;
begin

    Result := ObjectA.pDistanceFunc(P_) - R;

end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

function TMyChamfer.RayCasts( var WorldRay_:TRayRay; var WorldHit_:TRayHit ) :Boolean;
begin
     Result := RayCast( WorldRay_, WorldHit_ );

     //Result := RayCastChilds( WorldRay_, WorldHit_ ) or Result;
end;


constructor TMyChamfer.Create;
begin
     inherited;

     ObjectA := nil;
end;

destructor TMyChamfer.Destroy;
begin

     inherited;
end;







//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■