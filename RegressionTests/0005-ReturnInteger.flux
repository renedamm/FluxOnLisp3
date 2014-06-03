
module Test
{
	abstract type Object;
	abstract immutable type Number;
	final sealed immutable type Integer : Number;

	method Test() : Integer
	{
		return 0;
	}
}
