
module Test
{
	abstract type Object;
	abstract immutable type Number;
	final sealed immutable type Integer : Number;

	/*#RESULT: Value=0 */
	method Test() : Integer
	{
		return 0;
	}
}
