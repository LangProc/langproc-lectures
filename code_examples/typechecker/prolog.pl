likes(shreeya, italian).
likes(ata, japanese).
likes(aadin, lebanese).

cancook(momo, italian).
cancook(enxing, japanese).

wouldgeton(X,Y) :-
	likes(X,Z),
	cancook(Y,Z).