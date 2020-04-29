using Microsoft.Win32;
using P4Verification.Model;
using P4Verification.ViewModel;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;

namespace P4Verification
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        private VerificationModel model;
        private VerificationViewModel viewModel;
        private MainWindow window;

        public App()
        {
            Startup += new StartupEventHandler(App_Startup);
        }

        private void App_Startup(object sender, StartupEventArgs e)
        {
            model = new VerificationModel();

            viewModel = new VerificationViewModel(model);
            viewModel.ReadInput += new EventHandler(ViewModel_ReadInput);

            window = new MainWindow();
            window.DataContext = viewModel;
            window.Show();
        }

        private void ViewModel_ReadInput(object sender, System.EventArgs e)
        {
            OpenFileDialog openFileDialog = new OpenFileDialog();
            openFileDialog.DefaultExt = ".p4";
            openFileDialog.Filter = "P4 Files|*.p4";

            if (openFileDialog.ShowDialog() == true)
            {
                if ((_ = openFileDialog.OpenFile()) != null)
                {
                    string fileName = openFileDialog.FileName;
                    viewModel.Input = File.ReadAllText(fileName);
                    viewModel.OnPropertyChanged("Input");
                }
            }
        }
    }
}
