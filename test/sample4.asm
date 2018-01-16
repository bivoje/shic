CNT	START	256

NUM	WORD	100		THE NUMBER TO EXAMINE
MASK	WORD	1		MASK FOR BIT TESTING
COUNT	WORD	0		ONE-WORD VARIABLE INITIALIZED TO 0
ZERO	WORD	0		CONSTANT ZERO
ONE	WORD	1		CONSTANT ONE
DEV	BYTE	X'06'		OUTPUT DEVICE NUMBER

main	J	func

func	LDA	MASK		LOAD THE NUMBER TO `A` 

loopf	COMP	ZERO		COMPARE `MASK` WITH 0
	JEQ	done		DONE IF `MASK` HAS OVERFLOWED
	AND	NUM		A = mask & x
	COMP	ZERO		COPARE THE BIT AT THE MASK
	JEQ	noinc		SKIP INCREASING WHEN THE BIT IS 0
	LDA	COUNT		LOAD COUNT
	ADD	ONE		INCREASE COUNT BY ONE
	STA	COUNT		SAVE TO COUNT
noinc	LDA	MASK		LOAD THE MASK AGAIN
	ADD	MASK		DOUBLE THE MASK
	STA	MASK		SAVE MASK
	J	loopf

done	TD	DEV		TEST DEVICE
	JEQ	done
	LDCH	COUNT
	WD	DEV		WRITE TO DEVICE

halt	J	0
	END	main

.	int main()
.	{
.	  int x = ??;
.	  int count = 0;
.         int mask = 1;
.         while(mask)
.	  {
.	    count += mask & x;
.           mask += mask;
.	  }
.	}
.	
.	we need 3 variable x, count ,k
.	but general registers are only 2 S, T
.	A = k
.	S = 0
.	T = x COUNT = count
