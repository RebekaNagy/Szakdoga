using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;
using P4Verification;

namespace P4Checking.Model
{
    class HaskellCalculation : IDisposable
    {
        #region DLL imports

        [DllImport("Calculation.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern unsafe void hs_init(IntPtr argc, IntPtr argv);

        [DllImport("Calculation.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern unsafe void hs_exit();

        [DllImport("Calculation.dll", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
        private static extern unsafe char* cCalculate([MarshalAs(UnmanagedType.LPWStr)]string p, [MarshalAs(UnmanagedType.LPWStr)]string c);

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

        public string HsCalculate(string program, string conditions)
        {
            unsafe
            {
                String result = new String(cCalculate(program, conditions));
                return result;
            }
        }

        #endregion
    }
}
