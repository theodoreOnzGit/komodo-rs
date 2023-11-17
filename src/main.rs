mod sdata;
mod io;
mod control;
mod trans;

use std::time::SystemTime;

use sdata::get_time;
use uom::si::f64::*;

use crate::io::{inp_read, InputReadStruct};

fn main() {

    // translating
    // REAL(DP) :: st, fn, tot_time
    // I don't like the short name, so i'm giving it more explicit names
    // also, they are all dimensioned now
    let read_input_start: Time;
    let read_input_finish: Time;
    let total_time_elapsed: Time;
    let sys_time = SystemTime::now();
    // Read Input
    read_input_start = get_time(sys_time);

    let io_read: InputReadStruct = inp_read();

    read_input_finish = get_time(sys_time);

    let input_time = read_input_finish - read_input_start;

    // fortran code:
    //
    // ! terminal input
    // if (scr) then
    //   write(*,*)
    //   write(*,*) ' reading input ... done'
    // end if
    //
    //
    let scr: bool = io_read.io_options.scr;

    if scr {
        println!(" \n");
        println!(" reading input ... done ");
    }
    //


    // fortran code: 
    //
    // ! user will give the mode for calculation
    // SELECT CASE(mode)
    //     CASE('FIXEDSRC')
    //         CALL fixedsrc()
    //     CASE('ADJOINT')
    //         CALL adjoint()
    //     CASE('RODEJECT')
    //         IF (bther == 0) THEN
    //             CALL rod_eject()
    //         ELSE
    //             CALL rod_eject_th()
    //         END IF
    //     CASE('BCSEARCH')
    //         IF (bther == 0) THEN
    //             CALL cbsearch()
    //         ELSE
    //             CALL cbsearcht()
    //         END IF
    //     CASE DEFAULT
    //         CALL forward()
    // END SELECT
    //
    // I'll use an enum and match 
    // the mode is within the sdata module
    // the normally a char of length 100 within fortran
    // i don't want to do that, so I'll use an enum instead
    //
    // Ideally, the user should have an interface or something 
    // which converts the string into a proper node enum


    


    println!("Hello, world!");
}

