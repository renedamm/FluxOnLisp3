[ Description( "" ) ]
[ Copyright( "Copyright (C) 2012-2016 Rene Damm" ) ]
[ Author( "Rene Damm" ) ]
[ License( "Permission is hereby granted, free of charge, to any person obtaining a copy of this software "
		   "and associated documentation files (the \"Software\"), to deal in the Software without restriction, "
		   "including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, "
		   "and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, "
		   "subject to the following conditions:\n"
		   "\n"
		   "The above copyright notice and this permission notice shall be included in all copies or substantial "
		   "portions of the Software.\n"
		   "\n"
		   "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT "
		   "LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN "
		   "NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, "
		   "WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE "
		   "SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE." ) ]

library System
{
	import module System::Object;
	import module System::Runtime;
	import module System::Runtime::Host;


	//Write( "Hello, World" ).IntoStream( Runtime.Host.Console ); == To( Write( "Hello, World" ), Runtime.Host.Console )
	//Write( value ).Into( somewhere );

	//TEMP
    function Write : ( Object, Object ) -> ();
}

