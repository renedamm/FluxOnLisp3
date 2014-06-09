
module Test
{
	abstract type Object;
	abstract immutable type Number;
	final sealed immutable type Integer : Number;

	/*#CALL: Function=Test Result=2 */
	method Test() : Integer
	{
		return 0;
	}
}
