using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Text;

namespace P4Verification.ViewModel
{
    public class Condition
    {
        public ObservableCollection<string> ConditionOptions { get; set; }
        public List<string> ConditionForHaskell { get; set; }
        public Condition() { }
    }

    public class SelectCondition : Condition
    {
        public SelectCondition()
        {
            ConditionOptions = new ObservableCollection<string>(new List<string> { 
                "Nincs ellenőrzés", 
                "Összes feltételbeli változó valid",
                "Összes feltételbeli változó invalid"
            });
        }
    }

    public class TableCondition : Condition
    {
        public TableCondition()
        {
            ConditionOptions = new ObservableCollection<string>(new List<string> {
                "Nincs ellenőrzés",
                "Összes kulcs valid",
                "Összes kulcs invalid"
            });
        }
    }

    public class AssignmentCondition : Condition
    {
        public AssignmentCondition()
        {
            ConditionOptions = new ObservableCollection<string>(new List<string> {
                "Nincs ellenőrzés",
                "Bal oldali változó valid",
                "Bal oldali változó invalid",
                "Jobb oldali változók validak",
                "Jobb oldali változók invalidak",
                "Összes változó valid",
                "Összes változó invalid"
            });
        }
    }

    public class SetHeaderCondition : Condition
    {
        public SetHeaderCondition()
        {
            ConditionOptions = new ObservableCollection<string>(new List<string> {
                "Nincs ellenőrzés",
                "Fejléc valid",
                "Fejléc invalid",
                "Fejléc mezői validak",
                "Fejléc mezői invalidak",
                "Fejléc és a mezői validak",
                "Fejléc és a mezői invalidak"
            });
        }
    }

    public class DropCondition : Condition
    {
        public DropCondition()
        {
            ConditionOptions = new ObservableCollection<string>(new List<string> {
                "Nincs ellenőrzés",
                "Drop érték invalid",
                "Összes fejléc valid",
                "Összes fejléc invalid",
                "Összes mező valid",
                "Összes mező invalid"
            });
        }
    }
}
