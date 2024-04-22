!taken from https://gist.github.com/komasaru/f1e76fc44cd650298e2e9485620533ed

!lfortran linear-regression/linear-regression.f90 -c --generate-object-code --skip-pass unused_functions --target=wasm32-unknown-emscripten

!em++ linear-regression.o /Users/ubaid/Desktop/OpenSource/lfortran/src/bin/../runtime/lfortran_runtime_wasm_emcc.o -o linear-regression/linear-regression.js -sEXPORTED_FUNCTIONS='_malloc','_free','_calc_reg_line' -sWASM_BIGINT -sSTACK_SIZE=50mb -sINITIAL_MEMORY=256mb
subroutine calc_reg_line(x, y, n, a, b)
    implicit none
    integer,     parameter :: SP = kind(1.0)
    integer(SP), parameter :: DP = selected_real_kind(9, 99)
    integer(SP), intent(in) :: n
    real(DP), intent(in)  :: x(n), y(n)
    real(DP), intent(out) :: a, b
    integer(SP) :: size_x, size_y
    real(DP)    :: sum_x, sum_y, sum_xx, sum_xy

    size_x = size(x)
    size_y = size(y)

    if (size_x == 0 .or. size_y == 0) then
      print *, "[ERROR] array size == 0"
      stop
    end if
    if (size_x /= size_y) then
      print *, "[ERROR] size(X) != size(Y)"
      stop
    end if

    sum_x  = sum(x)
    sum_y  = sum(y)
    sum_xx = sum(x * x)
    sum_xy = sum(x * y)
    a = (sum_xx * sum_y - sum_xy * sum_x) &
    & / (size_x * sum_xx - sum_x * sum_x)
    b = (size_x * sum_xy - sum_x * sum_y) &
    & / (size_x * sum_xx - sum_x * sum_x)

end subroutine calc_reg_line
