! Program to calculate the time period of a simple pendulum on Earth
! Using the formula T = 2π√(L/g) where:
! - T = period (seconds)
! - L = pendulum length (meters)
! - g = acceleration due to gravity (m/s²)
!
! This implementation follows the small angle approximation for simple harmonic motion
! Assumes the pendulum oscillates with small angles (typically < 15 degrees)
program pendulum_period_calculator
    implicit none
    real, parameter :: pi = 3.14159265358979323846
    real, parameter :: gravitational_acceleration_earth = 9.80665  ! m/s² (standard gravity)
    real :: pendulum_length
    real :: pendulum_period
    
    ! Get input from user
    write(*,*) 'Pendulum Period Calculator'
    write(*,*) '=========================='
    write(*,*) 'Enter pendulum length (m): '
    read(*,*) pendulum_length
    
    ! Validate input
    if (pendulum_length <= 0.0) then
        write(*,*) 'Error: Pendulum length must be positive'
        stop
    end if
    
    ! Calculate the period using the formula T = 2π√(L/g)
    ! Where:
    ! - L is the length of the pendulum
    ! - g is the gravitational acceleration on Earth
    ! - T is the time period in seconds
    pendulum_period = 2.0 * pi * sqrt(pendulum_length / gravitational_acceleration_earth)
    
    ! Display results
    write(*,*) '=========================='
    write(*,*) 'CALCULATION RESULTS'
    write(*,*) '=========================='
    write(*,*) 'Pendulum length:', pendulum_length, 'm'
    write(*,*) 'Gravitational acceleration:', gravitational_acceleration_earth, 'm/s²'
    write(*,*) 'Period:', pendulum_period
    write(*,*) 'Period:', pendulum_period * 60.0, 'seconds'
    write(*,*) '=========================='
    
end program pendulum_period_calculator
