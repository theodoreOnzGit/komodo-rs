use core::time;
use std::thread;

use crate::sdata::CalculationMode;

/// reads input somehow
pub fn inp_read() -> InputReadStruct {
    // from fortran code:
    // IMPLICIT NONE
    // SAVE
    // 
    // IMPLICIT NONE disables 
    // implicit typing, not needed to translate
    //
    // SAVE essentially lets the variable get saved 
    // across multiple files. Sort of like global variables 
    // I'm not doing this as I strongly favour functional 
    // or object oriented programming
    //
    // in general, static variables are discouraged
    // construct various structs and return 

    let io_options = InputOutputOptions::default();

    // fortran:
    // CHARACTER(LEN=100) :: iname, oname
    //
    // ! ind is sed to read x indicator in beginning of input buffer line.
    // ! This to prevent reading next line
    // CHARACTER(LEN=1) :: ind

    let iname: String;
    let oname: String;
    let ind: char;

    let mode: CalculationMode;

    // start reading lots of input
    //
    // fortran:
    // ! Input, output and buffer input file unit number
    // INTEGER, PARAMETER :: iunit = 100   !input file unit number
    // INTEGER, PARAMETER :: ounit = 101   !output file unit number
    // INTEGER, PARAMETER :: buff  = 99    !input buffer file unit number (entire input)

    let iunit: i32 = 100;
    let ounit: i32 = 100;
    let buff: i32 = 99;

    // fortran: 
    //
    // ! Input buffer file unit number for each card
    // INTEGER, PARAMETER :: umode = 111, uxsec = 112, ugeom = 113, ucase = 114
    // INTEGER, PARAMETER :: uesrc = 115, uiter = 116, uprnt = 117, uadf  = 118
    // INTEGER, PARAMETER :: ucrod = 119, ubcon = 120, uftem = 121, umtem = 122
    // INTEGER, PARAMETER :: ucden = 123, ucbcs = 124, uejct = 125, uther = 126
    // INTEGER, PARAMETER :: uxtab = 127, ukern = 128, uextr = 129, uthet = 130
    // INTEGER, PARAMETER :: uoutp = 131
    // INTEGER :: bunit

    let umode: i32 = 111;  
    let uxsec: i32 = 112;  
    let ugeom: i32 = 113;  
    let ucase: i32 = 114;
    let uesrc: i32 = 115;  
    let uiter: i32 = 116;  
    let uprnt: i32 = 117;  
    let uadf : i32 = 118;
    let ucrod: i32 = 119;  
    let ubcon: i32 = 120;  
    let uftem: i32 = 121;  
    let umtem: i32 = 122;
    let ucden: i32 = 123;  
    let ucbcs: i32 = 124;  
    let uejct: i32 = 125;  
    let uther: i32 = 126;
    let uxtab: i32 = 127;  
    let ukern: i32 = 128;  
    let uextr: i32 = 129;  
    let uthet: i32 = 130;
    let uoutp: i32 = 131;
    let bunit: i32;

    // sort of translation
    // INTEGER :: bmode = 0, bxsec = 0, bgeom = 0, bcase = 0, besrc = 0
    // INTEGER :: biter = 0, bprnt = 0, badf  = 0, bcrod = 0, bbcon = 0
    // INTEGER :: bftem = 0, bmtem = 0, bcden = 0, bcbcs = 0, bejct = 0
    // INTEGER :: bther = 0, bxtab = 0, bkern = 0, bextr = 0, bthet = 0
    // INTEGER :: boutp = 0
    //
    // I changed it into an active/inactive enum

    let bmode = CardActive::Inactive; 
    let bxsec = CardActive::Inactive; 
    let bgeom = CardActive::Inactive; 
    let bcase = CardActive::Inactive; 
    let besrc = CardActive::Inactive;
    let biter = CardActive::Inactive; 
    let bprnt = CardActive::Inactive; 
    let badf  = CardActive::Inactive; 
    let bcrod = CardActive::Inactive; 
    let bbcon = CardActive::Inactive;
    let bftem = CardActive::Inactive; 
    let bmtem = CardActive::Inactive; 
    let bcden = CardActive::Inactive; 
    let bcbcs = CardActive::Inactive; 
    let bejct = CardActive::Inactive;
    let bther = CardActive::Inactive; 
    let bxtab = CardActive::Inactive; 
    let bkern = CardActive::Inactive; 
    let bextr = CardActive::Inactive; 
    let bthet = CardActive::Inactive;
    let boutp = CardActive::Inactive;

    // direct translation:
    // INTEGER :: g, i, N

    let i: i32;
    let g: i32;
    let command_argument_count: i32;


    // direct translation: 
    //
    // !Got this trick from: http://web.utah.edu/thorne/computing/Handy_Fortran_Tricks.pdf
    // N = command_argument_count()
    // IF (N < 1) THEN
    //    WRITE(*,*) '  NOTE : You can also write the input directly after the command'
    //    WRITE(*,'(A,A100)',ADVANCE='NO') '  INPUT NAME : '
    //    READ(*,*) iname
    // ELSE
    //    CALL get_command_argument(1,iname) !Grab the first command line argument
    // ENDIF
    // how does one do input and output in Rust?


    let io_struct = InputReadStruct {
        io_options
    };

    return io_struct;

}

/// contains all the information necessary for the input read
#[derive(Debug,PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct InputReadStruct {
    pub io_options: InputOutputOptions
}

// Translation of fortran code:
// !Ouput options
// LOGICAL, PARAMETER :: ogeom = .TRUE.  ! Geometry output print option
// LOGICAL, PARAMETER :: oxsec = .TRUE.  ! Macroscopic CXs output print option
// LOGICAL, PARAMETER :: scr = .TRUE.    ! Terminal ouput print option
#[derive(Debug,PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct InputOutputOptions {
    pub ogeom: bool,
    pub oxsec: bool,
    pub scr: bool,
}

impl Default for InputOutputOptions {
    fn default() -> Self {
        Self { ogeom: true, oxsec: true, scr: true }
    }
}


#[derive(Debug,PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum CardActive {
    Active,
    Inactive
}

impl Default for CardActive {
    fn default() -> Self {
        CardActive::Inactive
    }
}

impl Into<bool> for CardActive {
    fn into(self) -> bool {
        match self {
            CardActive::Active => true,
            CardActive::Inactive => false,
        }
    }
}
