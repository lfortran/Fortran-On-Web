!lfortran linear-regression/linear-regression.f90 -c --generate-object-code --skip-pass unused_functions --target=wasm32-unknown-emscripten

!em++ linear-regression.o /Users/ubaid/Desktop/OpenSource/lfortran/src/bin/../runtime/lfortran_runtime_wasm_emcc.o -o linear-regression/linear-regression.js -sEXPORTED_FUNCTIONS='_malloc','_free','_calculate_regression_line' -sWASM_BIGINT -sSTACK_SIZE=50mb -sINITIAL_MEMORY=256mb
subroutine calculate_regression_line(x_values, y_values, num_values, slope, intercept)
    implicit none
    integer, parameter :: dp = selected_real_kind(9, 99)
    integer, intent(in) :: num_values
    real(dp), intent(in)  :: x_values(num_values), y_values(num_values)
    real(dp), intent(out) :: slope, intercept
    integer :: x_size, y_size
    real(dp) :: sum_x, sum_y, sum_x_squared, sum_xy

    x_size = size(x_values)
    y_size = size(y_values)

    if (x_size == 0 .or. y_size == 0) then
      print *, "Error: Array size is 0."
      stop
    end if
    if (x_size /= y_size) then
      print *, "Error: Sizes of X and Y arrays are different."
      stop
    end if

    sum_x = sum(x_values)
    sum_y = sum(y_values)
    sum_x_squared = sum(x_values * x_values)
    sum_xy = sum(x_values * y_values)

    slope = (x_size * sum_xy - sum_x * sum_y) &
        & / (x_size * sum_x_squared - sum_x * sum_x)
    intercept = (sum_x_squared * sum_y - sum_xy * sum_x) &
        & / (x_size * sum_x_squared - sum_x * sum_x)
end subroutine calculate_regression_line
