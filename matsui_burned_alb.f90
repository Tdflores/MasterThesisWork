 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES
  2 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
  3 
  4  module module_albedo_wildfire
  5  implicit none
  6 
  7 !---------------------------------------------------------------------------------------------------
  8 ! NASA GSFC makes no representations about the suitability of software for any purpose. 
  9 ! It is provided as is without express or implied warranty. Neither NASA GSFC (the US 
 10 ! government) nor Principal Developers (their organizations) shall be liable for any 
 11 ! damages suffered by the user of this software. In addition, please do not distribute 
 12 ! the software to third party.
 13 !
 14 ! Comments:  
 15 !   This module purturbe surface albedo for a given wildfire fraction. 
 16 !   The Look-Up-Table (LUT) of buruned area albedo is provided by Dr. Charles Gatabe @ NASA GSFC.
 17 !
 18 !   Instrument: CAR (NASA's Cloud Absorption Radiometer)
 19 !   Field campaign – Skukuza field campaign. 
 20 !   Area: South Africa – savannah  [Lat:25.23260 deg South, Lon: 31:303376 deg East]
 21 !   date: May 20, 2005
 22 !  
 23 !
 24 ! ---subroutine perturb_albedo_wildfire---
 25 ! Usage Derive spectrum burned albedo, which require following inputs/output. 
 26 !
 27 !  real :: wavel_in       !wavelength [um]
 28 !  real :: albedo_in      ! normal albedo w/o wildfire  (0.~ 1.)
 29 !  real :: frac_wildfire  ! pixel fraction of wildfires (0.~ 1.)
 30 !  real :: albedo_out     ! purturbed albedo after wildfire (0.~ 1.)
 31 !  real :: albedo_burned  ! albedo of just burned area (0.~ 1.)
 32 !                     
 33 !  call perturb_albedo_wildfire(wavel_in, albedo_in, frac_wildfire, albedo_out, albedo_burned)
 34 !
 35 ! 
 36 ! -- subroutine perturb_albedo_wildfire_broadband ----
 37 ! Usage: Derive broadband burned albedo, which requires following inputs/outputs.
 38 !
 39 !  character(len=10) :: typ_band ! UV, VIS, NIR, UVVIS, VISNIR, or UVVISNIR depending on the radiation band you want. 
 40 !  real :: wavel_in       !wavelength [um]
 41 !  real :: albedo_in      ! normal albedo w/o wildfire  (0.~ 1.)
 42 !  real :: frac_wildfire  ! pixel fraction of wildfires (0.~ 1.)
 43 !  real :: albedo_out     ! purturbed albedo after wildfire (0.~ 1.)
 44 !  real :: albedo_burned  ! albedo of just burned area (0.~ 1.)
 45 !  For example if you want to broandband shortwave radiation (from UV to NIR spectrum) --> UVVISNIR
 46 !              if you want to VIS band radiation --> VIS
 47 !
 48 !  call perturb_albedo_wildfire_broadband(trim(typ_band), albedo_in, frac_wildfire, albedo_out, albedo_burned)
 49 !
 50 !
 51 ! History: 
 52 !  08/2015  Toshi Matsui@NASA GSFC ; Initial.
 53 !           
 54 ! References: 
 55 !---------------------------------------------------------------------------------------------------
 56  save     ! all module parameters will be saved
 57 
 58 !
 59 ! Encapsulation control 
 60 !
 61  private   ! encapsulate all variables and subourtines (non accessible)
 62 
 63 !
 64 ! public (accessible) subroutines
 65 !
 66  public :: perturb_albedo_wildfire, &
 67            perturb_albedo_wildfire_broadband
 68 
 69 
70 !
 71 ! This Look-Up-Table (LUT) buruned area albedo is provided by Dr. Charles Gatabe @ NASA GSFC.
 72 !
 73 ! Instrument: CAR (NASA's Cloud Absorption Radiometer)
 74 ! Field campaign – Skukuza field campaign. 
 75 ! Area: South Africa – savannah  [Lat:25.23260 deg South, Lon: 31:303376 deg East]
 76 ! date: May 20, 2005
 77 !
 78 ! More details on the flight: http://car.gsfc.nasa.gov/data/index.php?id=131&mis_id=10&n=Skukuza
 79 !
 80 
 81  integer,parameter :: nband_sw = 8
 82 
 83  real,parameter :: lut_wavel_albedo(nband_sw) = &  ! anchor points wavelength [um]
 84     (/0.34, 0.381, 0.472, 0.682, 0.87, 1.036 , 1.219 , 1.273/)
 85 
 86  real,parameter :: lut_albedo_burned(nband_sw) = &  ! burned-area spectrum albedo [-]
 87     (/0.02041, 0.02615, 0.02602, 0.04159, 0.06536, 0.07777 , 0.08785 , 0.08511/)
 88 
 89 ! real,parameter :: lut_albedo_natural(nband_sw) = &  ! Savannah natural spectrum albedo w/o wildfire  [-]
 90 !    (/0.02228, 0.03176, 0.03873, 0.09484, 0.18488, 0.21797 , 0.23876 , 0.24560/)
 91 
 92 !
 93 ! Derive broadband burned albedo LUT using Plank's Law using above spectrum albedo
 94 ! for UV, VIS, NIR, UVVIS, VISNIR, UVVISNIR range
 95 !
 96 
 97  contains
 98 
 99 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
100 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
101 
102  subroutine perturb_albedo_wildfire_broadband ( type_band, albedo_in, frac_wildfire,  albedo_out , albedo_burned )
103  implicit none
104 !---------------------------------------------------------------------------------------------------
105 ! Comments:  
106 !   This module purturbe broadband surface albedo for a given wildfire fraction. 
107 !   Make sure input/output albedo units are fraction (0.0 ~ 1.0), NOT in %. 
108 !
109 ! History: 
110 !  08/2015  Toshi Matsui@NASA GSFC ; Initial.
111 !           
112 ! References: 
113 !---------------------------------------------------------------------------------------------------
114  character(len=*),intent(in) :: type_band ! wavelength of albedo [um]
115  real,intent(in) :: albedo_in      ! normal albedo w/o wildfire  (0.~ 1.)
116  real,intent(in) :: frac_wildfire  ! pixel fraction of wildfires (0.~ 1.)
117  real,intent(out) :: albedo_out    ! purturbed albedo after wildfire (0.~ 1.)
118  real,intent(out) :: albedo_burned ! interpolated burned-area albedo at input wavelength [-]
119 
120  integer :: n
121  integer :: nband_start , nband_end
122  real :: wave_m  ![m]
123  real :: Bt, Bt_sum , alb_sum
124 
125 !Boltzman's constant
126  real,parameter :: c1 = 3.74e-16  !1st constant [W m2]
127  real,parameter :: c2 = 1.439e-2  !2nd constant [m K]
128  real,parameter :: t_sun = 6100.  !solar radiative temperature [K]
129 
130 !
131 ! default output equal to the input
132 !
133   albedo_out = albedo_in
134 
135 !
136 !
137 ! check input first
138 !
139 
140  select case(trim(type_band))
141  case('UV')
142   nband_start = 1 ; nband_end = 2
143  case('VIS')
144   nband_start = 3 ; nband_end = 4
145  case('NIR')
146   nband_start = 5 ; nband_end = 8
147  case('UVVIS')
148   nband_start = 1 ; nband_end = 4
149  case('VISNIR')
150   nband_start = 3 ; nband_end = 8
151  case('UVVISNIR')
152   nband_start = 1 ; nband_end = 8
153  case default
154    print*,'MSG perturb_albedo_wildfire_broad: Warning no such type of type_band. Return'
155    return
156  end select
157 
158  if( albedo_in < 0.e0 ) then
159     print*,'MSG perturb_albedo_wildfire_broad: Warning input albedo is negative. Return'
160     return
161  elseif( albedo_in > 1.e0 ) then
162     print*,'MSG perturb_albedo_wildfire_broad: Warning input albedo is greater than 1.0. Return'
163     return
164  endif
165 
166  if( frac_wildfire < 0.e0 ) then
167     print*,'MSG perturb_albedo_wildfire_broad: Warning input frac_wildfire is negative. Return'
168     return
169  elseif( frac_wildfire > 1.e0 ) then
170     print*,'MSG perturb_albedo_wildfire_broad: Warning input frac_wildfire is greater than 1.0. Return'
171     return
172  endif
173 
174  if( frac_wildfire == 0.e0 ) then
175    return
176  endif
177 
178 !
179 ! integrate spectrum albedo with weight of solar radiation flux via Plank's Law
180 !
181  Bt_sum = 0.  ; alb_sum = 0.
182  sw_loop: do n = nband_start, nband_end
183     wave_m = lut_wavel_albedo(n) * 1.e-6 ![m]
184     Bt = ( c1 / ( wave_m ** 5 ) )/ ( exp(c2/(wave_m*t_sun))-1.e0 ) * 1.e-12  !Plank's Law [Mega Wm-2 um-1]
185     Bt_sum = Bt_sum + Bt   !integrate weight 
186     alb_sum = alb_sum + Bt * lut_albedo_burned(n) !integrate albedo
187  enddo sw_loop
188 
189  albedo_burned = alb_sum / Bt_sum  ! broadband burned albedo [-]
190 
191 !
192 ! output perturbed albedo 
193 !
194  if( frac_wildfire == 1.e0 ) then
195    albedo_out = albedo_burned  ! [-]
196  else
197    albedo_out = frac_wildfire * albedo_burned + (1.-frac_wildfire) * albedo_in  ! [-]
198  endif
199 
200  end subroutine perturb_albedo_wildfire_broadband
201 
202 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
203 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
204 
205  subroutine perturb_albedo_wildfire ( wavel_in, albedo_in, frac_wildfire,  albedo_out , albedo_burned )
206  implicit none
207 !---------------------------------------------------------------------------------------------------
208 ! Comments:  
209 !   This module purturbe spectrum surface albedo for a given wildfire fraction. 
210 !   Make sure input/output albedo units are fraction (0.0 ~ 1.0), NOT in %. 
211 !
212 ! History: 
213 !  08/2015  Toshi Matsui@NASA GSFC ; Initial.
214 !           
215 ! References: 
216 !---------------------------------------------------------------------------------------------------
217  real,intent(in) :: wavel_in       ! wavelength of albedo [um]
218  real,intent(in) :: albedo_in      ! normal albedo w/o wildfire  (0.~ 1.)
219  real,intent(in) :: frac_wildfire  ! pixel fraction of wildfires (0.~ 1.)
220  real,intent(out) :: albedo_out    ! purturbed albedo after wildfire (0.~ 1.)
221  real,intent(out) :: albedo_burned ! interpolated burned-area albedo at input wavelength [-]
222 
223  integer :: n
224  real :: wgt1, wgt2
225 
226 !
227 ! default output equal to the input
228 !
229   albedo_out = albedo_in
230 
231 !
232 !
233 ! check input first
234 !
235  if( wavel_in < 0.e0 ) then
236     print*,'MSG perturb_albedo_wildfire: Warning input wavelength is negative. Return'
237  endif
238 
239  if( albedo_in < 0.e0 ) then
240     print*,'MSG perturb_albedo_wildfire: Warning input albedo is negative. Return'
241     return
242  elseif( albedo_in > 1.e0 ) then
243     print*,'MSG perturb_albedo_wildfire: Warning input albedo is greater than 1.0. Return'
244     return
245  endif
246 
247  if( frac_wildfire < 0.e0 ) then
248     print*,'MSG perturb_albedo_wildfire: Warning input frac_wildfire is negative. Return'
249     return
250  elseif( frac_wildfire > 1.e0 ) then
251     print*,'MSG perturb_albedo_wildfire: Warning input frac_wildfire is greater than 1.0. Return'
252     return
253  endif
254 
255  if( frac_wildfire == 0.e0 ) then
256    return
257  endif
258 
259 
260 !
261 ! interpolate burned area albedo for a given wavelength
262 !
263  if( wavel_in <= lut_wavel_albedo(1) ) then
264 
265     albedo_burned = lut_albedo_burned(1)
266 
267  elseif( wavel_in >= lut_wavel_albedo(nband_sw) )  then
268 
269     albedo_burned = lut_albedo_burned(nband_sw)
270 
271  else
272 
273     sw_loop: do n = 1, nband_sw-1
274 
275        if( wavel_in >= lut_wavel_albedo(n) .and. wavel_in <= lut_wavel_albedo(n+1) ) then
276            wgt1 = ( lut_wavel_albedo(n+1) - wavel_in ) / ( lut_wavel_albedo(n+1) - lut_wavel_albedo(n) )
277            wgt2 = 1.-wgt1
278            albedo_burned = wgt1 * lut_albedo_burned(n) + wgt2 * lut_albedo_burned(n+1)
279            exit sw_loop
280        endif
281 
282     enddo sw_loop
283 
284  endif
285 
286 !
287 ! output perturbed albedo 
288 !
289  if( frac_wildfire == 1.e0 ) then
290    albedo_out = albedo_burned   ! [-]
291  else
292    albedo_out = frac_wildfire * albedo_burned + (1.-frac_wildfire) * albedo_in    ! [-]
293  endif
294 
295  end subroutine perturb_albedo_wildfire
296 
297 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
298 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
299 
300  end module module_albedo_wildfire
301 
302 ! Below is example main program to call these subroutines. T. Matsui
303 
304 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
305 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
306 
307  program main
308  use module_albedo_wildfire
309 
310  real :: wavel_in      !wavelength [um]
311  real :: albedo_in      ! normal albedo w/o wildfire  (0.~ 1.)
312  real :: frac_wildfire  ! pixel fraction of wildfires (0.~ 1.)
313  real :: albedo_out    ! purturbed albedo after wildfire (0.~ 1.)
314  real :: albedo_burned !burned albedo
315 
316  character(len=10) :: typ_band
317 
318  typ_band = 'VISNIR'
319  wavel_in = 0.5
320  albedo_in = 0.2
321  frac_wildfire = 0.5
322 
323  !
324  ! units: wavel_in [um]; 
325  ! albedo_in, frac_wildfire, albedo_out,albedo_burned are all fraction (0.0~1.0)
326  ! 
327  print*,'Call perturb_albedo_wildfire'
328  call perturb_albedo_wildfire(wavel_in, albedo_in, frac_wildfire, albedo_out, albedo_burned)
329 
330  write(*,FMT='(5F10.4)') wavel_in, albedo_in, frac_wildfire, albedo_out, albedo_burned
331 
332 
333  !
334  ! typ_band:  UV, VIS, NIR, UVVIS, VISNIR, or UVVISNIR depending on the radiation band you want. 
335  ! units: albedo_in, frac_wildfire, albedo_out, albedo_burned are all fraction (0.0~1.0)
336  ! For example if you want to broandband shortwave radiation (from UV to NIR spectrum) --> UVVISNIR
337  !             if you want to VIS band radiation --> VIS
338  !
339  print*, 'Call perturb_albedo_wildfire_broadband'
340  call perturb_albedo_wildfire_broadband(trim(typ_band), albedo_in, frac_wildfire, albedo_out, albedo_burned)
341 
342  write(*,FMT='(A10,1X,4F10.4)') trim(typ_band), albedo_in, frac_wildfire, albedo_out, albedo_burned
343 
344 
345  end program main
346 
347 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 
348 !WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES WILDFIRES 

