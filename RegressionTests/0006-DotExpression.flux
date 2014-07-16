
module Test
{
	abstract sealed type Object;
	final sealed type Integer;
	abstract type String;

	method ToString( Integer ) : String
	{
		return "1";
	}

	/*#CALL: Function=Test Result="1" */
	method Test() : String
	{
		return 1.ToString;
	}
}

