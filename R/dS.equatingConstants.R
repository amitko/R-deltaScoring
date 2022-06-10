dS.equatingConstants <- function(baseTestDeltas, newTestDeltas,commonItems)
{
	Zb = 1/1.702 * log(baseTestDeltas / (1 - baseTestDeltas));
	Zn = 1/1.702 * log(newTestDeltas / (1 - newTestDeltas));
	
	Zbc = Zb[commonItems[,1]];
	Znc = Zn[commonItems[,2]];
	
	A = std(Zbc)/std(Zbn);
	B = mean(Zbc) - A * mean(Zbn);
	
	return( list( "A" = A, "B" = B) );
	
}