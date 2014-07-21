
module Test
{
	abstract sealed type Object;
	type String;
	type Integer;

	method Callee( String ) : Integer
	{
		return 456;
	}

	/*#CALL: Function=Callee Result=456 */
	method Caller() : Integer
	{
		return Callee( "foo" );
	}
}

