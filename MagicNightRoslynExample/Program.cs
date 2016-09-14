using System;
//using StringFormatAttribute;

namespace MagicNightRoslynExample {
    internal class Program {

        //[StringFormat(formatParam: "formatString", argsParam: "arguments")]
        public static void Log(DateTime date, string formatString, params string[] arguments) {
            Console.WriteLine(date + ": " + string.Format(formatString, arguments));
        }

        static void Main(string[] args) {
            Log(DateTime.Now, "Hello, my name is {0} {1}", "Yuri");
        }
    }
}
