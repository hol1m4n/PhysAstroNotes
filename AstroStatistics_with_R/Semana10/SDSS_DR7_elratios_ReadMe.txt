DESCRIPTION OF FLAGS FOR EMISSION LINE CLASSIFICATION OF GALAXIES.

The term 'main lines' refers to the four lines required for BPT-main
diagnostic diagram ([OIII], Hbeta, [NII], Halpha)

flag_elines:
VALUE	CLASS / DIAGRAM			DESCRIPTION
0	Passive				None of the 4 main lines present.
1	Passive				Only 1 of the main lines present.
2	Unclassifiable			2 unmatching main lines present.
3	Unclassifiable			No Halpha,
					(with other 3 main lines present).
4	Unlcassifiable			No NII AND no OI AND no SII,
					(with other 3 main lines present).
5	Only Coz98			NII & Ha present,
					(but NO OIII OR Hb)
6	Only BPT-alt			No NII, but OI present.
					(with other 3 main lines present).
7	Only VO87			No NII, but (both) SII present.
					(with other 3 main lines present).
8	BPT-alt & VO87			No NII, but OI AND (both) SII present.
					(with other 3 main lines present).
9	BPT-main & Coz98		All main lines but NO OI NOR SII.
10	BPT-main, Coz98 & BPT-alt	All main lines AND OI present (NO SII).
11	BPT-main, Coz98 & VO87		All main lines AND SII present (NO OI).
12	BPT-main, Coz98 & BPT-alt	All lines present.

flag_bptmain:
Classes obtained using diagnostic lines of Kau03 and Kew01.
VALUE	CLASS	DESCRIPTION
0	NOMAIN	Cannot be classified by this diagram.
1	SF	Star-Forming galaxy.
2	AGN	AGN-host galaxy.
3	TO	Transition Object.

flag_bptalt:
Classes obtained using diagnostic line of Kew01.
VALUE	CLASS	DESCRIPTION
0	NOALT	Cannot be classified by this diagram.
1	SF	Star-Forming galaxy.
2	AGN	AGN-host galaxy.

flag_vo87:
Classes obtained using diagnostic line of Kew01.
VALUE	CLASS	DESCRIPTION
0	NOVO87	Cannot be classified by this diagram.
1	SF	Star-Forming galaxy.
2	AGN	AGN-host galaxy.
