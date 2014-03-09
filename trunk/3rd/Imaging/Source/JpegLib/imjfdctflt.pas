unit imjfdctflt;

{$N+}
{ This file contains a floating-point implementation of the
  forward DCT (Discrete Cosine Transform).

  This implementation should be more accurate than either of the integer
  DCT implementations.  However, it may not give the same results on all
  machines because of differences in roundoff behavior.  Speed will depend
  on the hardware's floating point capacity.

  A 2-D DCT can be done by 1-D DCT on each row followed by 1-D DCT
  on each column.  Direct algorithms are also available, but they are
  much more complex and seem not to be any faster when reduced to code.

  This implementation is based on Arai, Agui, and Nakajima's algorithm for
  scaled DCT.  Their original paper (Trans. IEICE E-71(11):1095) is in
  Japanese, but the algorithm is described in the Pennebaker & Mitchell
  JPEG textbook (see REFERENCES section in file README).  The following code
  is based directly on figure 4-8 in P&M.
  While an 8-point DCT cannot be done in less than 11 multiplies, it is
  possible to arrange the computation so that many of the multiplies are
  simple scalings of the final outputs.  These multiplies can then be
  folded into the multiplications or divisions by the JPEG quantization
  table entries.  The AA&N method leaves only 5 multiplies and 29 adds
  to be done in the DCT itself.
  The primary disadvantage of this method is that with a fixed-point
  implementation, accuracy is lost due to imprecise representation of the
  scaled quantization values.  However, that problem does not arise if
  we use floating point arithmetic. }

{ Original : jfdctflt.c ; Copyright (C) 1994-1996, Thomas G. Lane. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjpeglib,
  imjdct;		{ Private declarations for DCT subsystem }


{ Perform the forward DCT on one block of samples.}

{GLOBAL}
procedure jpeg_fdct_float (var data : array of FAST_FLOAT);

implementation

{ This module is specialized to the case DCTSIZE = 8. }

{$ifndef DCTSIZE_IS_8}
  Sorry, this code only copes with 8x8 DCTs. { deliberate syntax err }
{$endif}


{ Perform the forward DCT on one block of samples.}

{GLOBAL}
procedure jpeg_fdct_float (var data : array of FAST_FLOAT);
type
  PWorkspace = ^TWorkspace;
  TWorkspace = array [0..DCTSIZE2-1] of FAST_FLOAT;
var
  tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7 : FAST_FLOAT;
  tmp10, tmp11, tmp12, tmp13 : FAST_FLOAT;
  z1, z2, z3, z4, z5, z11, z13 : FAST_FLOAT;
  dataptr : PWorkspace;
  ctr : int;
begin
  { Pass 1: process rows. }

  dataptr := PWorkspace(@data);
  for ctr := DCTSIZE-1 downto 0 do
  begin
    tmp0 := dataptr^[0] + dataptr^[7];
    tmp7 := dataptr^[0] - dataptr^[7];
    tmp1 := dataptr^[1] + dataptr^[6];
    tmp6 := dataptr^[1] - dataptr^[6];
    tmp2 := dataptr^[2] + dataptr^[5];
    tmp5 := dataptr^[2] - dataptr^[5];
    tmp3 := dataptr^[3] + dataptr^[4];
    tmp4 := dataptr^[3] - dataptr^[4];

    { Even part }

    tmp10 := tmp0 + tmp3;	{ phase 2 }
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    dataptr^[0] := tmp10 + tmp11; { phase 3 }
    dataptr^[4] := tmp10 - tmp11;

    z1 := (tmp12 + tmp13) * ({FAST_FLOAT}(0.707106781)); { c4 }
    dataptr^[2] := tmp13 + z1;	{ phase 5 }
    dataptr^[6] := tmp13 - z1;

    { Odd part }

    tmp10 := tmp4 + tmp5;	{ phase 2 }
    tmp11 := tmp5 + tmp6;
    tmp12 := tmp6 + tmp7;

    { The rotator is modified from fig 4-8 to avoid extra negations. }
    z5 := (tmp10 - tmp12) * ( {FAST_FLOAT}(0.382683433)); { c6 }
    z2 := {FAST_FLOAT}(0.541196100) * tmp10 + z5; { c2-c6 }
    z4 := {FAST_FLOAT}(1.306562965) * tmp12 + z5; { c2+c6 }
    z3 := tmp11 * {FAST_FLOAT} (0.707106781); { c4 }

    z11 := tmp7 + z3;		{ phase 5 }
    z13 := tmp7 - z3;

    dataptr^[5] := z13 + z2;	{ phase 6 }
    dataptr^[3] := z13 - z2;
    dataptr^[1] := z11 + z4;
    dataptr^[7] := z11 - z4;

    Inc(FAST_FLOAT_PTR(dataptr), DCTSIZE);  { advance pointer to next row }
  end;

  { Pass 2: process columns. }

  dataptr := PWorkspace(@data);
  for ctr := DCTSIZE-1 downto 0 do
  begin
    tmp0 := dataptr^[DCTSIZE*0] + dataptr^[DCTSIZE*7];
    tmp7 := dataptr^[DCTSIZE*0] - dataptr^[DCTSIZE*7];
    tmp1 := dataptr^[DCTSIZE*1] + dataptr^[DCTSIZE*6];
    tmp6 := dataptr^[DCTSIZE*1] - dataptr^[DCTSIZE*6];
    tmp2 := dataptr^[DCTSIZE*2] + dataptr^[DCTSIZE*5];
    tmp5 := dataptr^[DCTSIZE*2] - dataptr^[DCTSIZE*5];
    tmp3 := dataptr^[DCTSIZE*3] + dataptr^[DCTSIZE*4];
    tmp4 := dataptr^[DCTSIZE*3] - dataptr^[DCTSIZE*4];

    { Even part }

    tmp10 := tmp0 + tmp3;	{ phase 2 }
    tmp13 := tmp0 - tmp3;
    tmp11 := tmp1 + tmp2;
    tmp12 := tmp1 - tmp2;

    dataptr^[DCTSIZE*0] := tmp10 + tmp11; { phase 3 }
    dataptr^[DCTSIZE*4] := tmp10 - tmp11;

    z1 := (tmp12 + tmp13) * {FAST_FLOAT} (0.707106781); { c4 }
    dataptr^[DCTSIZE*2] := tmp13 + z1; { phase 5 }
    dataptr^[DCTSIZE*6] := tmp13 - z1;

    { Odd part }

    tmp10 := tmp4 + tmp5;	{ phase 2 }
    tmp11 := tmp5 + tmp6;
    tmp12 := tmp6 + tmp7;

    { The rotator is modified from fig 4-8 to avoid extra negations. }
    z5 := (tmp10 - tmp12) * {FAST_FLOAT} (0.382683433); { c6 }
    z2 := {FAST_FLOAT} (0.541196100) * tmp10 + z5; { c2-c6 }
    z4 := {FAST_FLOAT} (1.306562965) * tmp12 + z5; { c2+c6 }
    z3 := tmp11 * {FAST_FLOAT} (0.707106781); { c4 }

    z11 := tmp7 + z3;		{ phase 5 }
    z13 := tmp7 - z3;

    dataptr^[DCTSIZE*5] := z13 + z2; { phase 6 }
    dataptr^[DCTSIZE*3] := z13 - z2;
    dataptr^[DCTSIZE*1] := z11 + z4;
    dataptr^[DCTSIZE*7] := z11 - z4;

    Inc(FAST_FLOAT_PTR(dataptr));   { advance pointer to next column }
  end;
end;

end.
