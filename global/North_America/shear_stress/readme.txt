--------------------------
2019/05/13
--------------------------

I need to improve the precision in some places, so I creating some new domains. The current largest number is 211, so the next domain is 212.

9 (St Laurence valley and Erie/Ontario basins). Split north of Lake Erie.
212 --> St Laurence Valley
9 --> Erie and Ontario basins

I kept 212 as the last shear stress value (50000) but lowered 9 to 20000. I set 10 (Michigan/Huron) to be 20000 as well

16 (Grenville) and 17 (Superior Province) are going to need to be split. Here it goes:

southern half of 17 is now a region I call "Clay Belt", which is a relatively flat area with lake sediment fill. Presumably it would have also been like that prior to the LGM. I am initially setting it to be the same shear stress as neighbouring 89 (50000)

17 --> Western Quebec Superior Province
213 --> Clay Belt

Southern Half of 16 will also be split into a region I am calling "Ottawa River". In general, this area is much lower relief than the Grenville province further north. I will also set this area to have a lower shear stress (50000)

16 --> Grenville Province in Quebec
214 --> Ottawa River

Splitting off the Strait of Georgia/Juan de Fuca Strait from 24 so that it can be a higher value.

215 --> Strait of Georgia - set to 50000 Pa

Increasing the moutainous areas by 20000 Pa, including 32 and 93

Also reducing Appalation Mountains and Nova Scotia (domains 7 and 5) so that it is easier to have a Laurentide centered ice flow direction

I also think that the values south of Hudson Bay need to be more similar, otherwise it is going to be trying to build a dome there. I put everything to be 30000 Pa.


--------------------------
2019/06/14
--------------------------


After playing around with the Earth models, I think using a higher lower mantle viscosity is needed in order to achieve proper ice volume values. I'm going to start with ehgr, which is 4x10^22. The first results will allow for much more ice in Hudson Bay by the looks of it, but I need to revert back to having the ice be thicker at later times for realism sake, I think.

78 - this is an ice stream, currently set to a very high 90000 Pa. Lowering this a lot, maybe 50000 to start
75 - this is a peninsula, it should be higher than 78

Made a lot of adjustments to ensure that the core region stays thicker longer, thus making it so the far field sea level doesn't rise too early


--------------------------
2019/06/14
--------------------------

I think the main thing I want to change is that I want the ice stream going west of Baffin to be much lower than it is now. Also want to keep the ice thickness higher in the Hudson Strait region to help fit the sea level curves. 78 is that ice stream

--------------------------
2019/08/12
--------------------------

I think it is prudent to have the ridge in Hudson Bay be on the east side of Hudson Bay during deglaciation. This will require adjustments to the shear stress in domains 24 and 92. I increased 24 and decreased 92. I also reduced 73.

I also think I am reducing the shear stress too early in most places, I am advancing it one time step in all cases. This should make for a more realistic reconstruction.



--------------------------
2019/08/16
--------------------------

After starting to plot up the classic "Eustatic" sites, I think it is safe to reduce the thickness of Hudson Bay more, which I am going to do by reducing the shear stress in Hudson Strait.

Another thing I am doing is increasing the shear stress in the Hudson Bay area at around 30000 yr BP. This will help the fit to global sea level at that time, which it is currently underestimating.

I also delayed the timing of reductions in shear stress in the Eastern parts of the Laurentide from 15000 to 12500, to try and match the global signal at that time.

Another thing I did was fix a problem with domain 23 (Foxe Basin), which was instantly dropping the shear stress to a minimal value at 30000 yr BP. I expect this will increase the volume at this time by quite a bit.

Edited the East Coast shear stress values so that they remain high at 15000

I was not happy with what was going on in domain 90, so I reduced it to be the same as domain 92.



--------------------------
2019/08/19
--------------------------

I think it can be justified to reduce the ice thickness in Hudson Bay further, which I am going to do by reducing Hudson Strait. I will also increase the shear stress in the lead up to the MIS 4 maximum in Hudson Bay. I also increased the Foxe Dome, and made it so that domain 90 was thicker at LGM than the last run


--------------------------
2019/08/20
--------------------------

The consequence of the last changes were that the sea levels along the Hudson Strait, especially Sugluk, are much lower now. I've increased the shear stress in domain 20 to try and help that situation. I've also done some reductions in the East Coast to try to bring down the sea level there.
