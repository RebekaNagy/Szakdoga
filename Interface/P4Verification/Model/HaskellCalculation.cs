using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using P4Verification;

namespace P4Verification.Model
{
    class HaskellCalculation : IDisposable
    {
        #region DLL imports

        [DllImport("..\\..\\..\\Calculation.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern unsafe void hs_init(IntPtr argc, IntPtr argv);

        [DllImport("..\\..\\..\\Calculation.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern unsafe void hs_exit();

        [DllImport("..\\..\\..\\Calculation.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern unsafe string cCalculate(string i);

        #endregion

        #region Public interface

        public HaskellCalculation()
        {
            unsafe { hs_init(IntPtr.Zero, IntPtr.Zero); }
        }

        public void Dispose()
        {
            unsafe { hs_exit(); }
        }

        public string HsCalculate(string i)
        {
            unsafe
            {
                var result = cCalculate(i);
                return result;
            }
        }

        #endregion
    }
}
