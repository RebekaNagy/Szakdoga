using System;
using System.Collections.Generic;
using System.Text;

namespace P4Verification.Model
{
    class ErrorEventArgs : EventArgs
    {
        public string ErrorString { get; private set; }

        public ErrorEventArgs(string errorString)
        {
            ErrorString = errorString;
        }
    }
}
