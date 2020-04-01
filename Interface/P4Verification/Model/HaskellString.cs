using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using P4Verification;

namespace P4Verification.Model
{
    class HaskellString : IDisposable
    {
        #region DLL imports

        [DllImport("..\\..\\..\\HSdll.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern unsafe void hs_init(IntPtr argc, IntPtr argv);

        [DllImport("..\\..\\..\\HSdll.dll", CallingConvention = CallingConvention.Cdecl)]
        private static extern unsafe void hs_exit();

        [DllImport("..\\..\\..\\HSdll.dll", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Unicode)]
        private unsafe static extern char* cstring([MarshalAs(UnmanagedType.LPWStr)]string i);

        #endregion

        #region Public interface

        public HaskellString()
        {
            unsafe { hs_init(IntPtr.Zero, IntPtr.Zero); }
        }

        public void Dispose()
        {
            unsafe { hs_exit(); }
        }

        public string CopyString(string i)
        {
            unsafe
            {
                String result = new String(cstring(i));
                return result;
            }
        }

        #endregion
    }
}