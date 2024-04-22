!lfortran matmul2d/matmul2d.f90 -c --generate-object-code --skip-pass unused_functions --target=wasm32-unknown-emscripten

!em++ matmul2d.o /Users/ubaid/Desktop/OpenSource/lfortran/src/bin/../runtime/lfortran_runtime_wasm_emcc.o -o matmul2d/matmul2d.js -sEXPORTED_FUNCTIONS='_malloc','_free','_matmul2d' -sWASM_BIGINT -sSTACK_SIZE=50mb -sINITIAL_MEMORY=256mb
subroutine matmul2d(matrix1, rows1, cols1, matrix2, rows2, cols2, matrix3)
    integer, intent(in) :: rows1, cols1, rows2, cols2
    double precision, intent(in) :: matrix1(rows1, cols1), matrix2(cols1, cols2)
    double precision, intent(out) :: matrix3(rows1, cols2)
    integer :: i, j, k
    do i = 1, rows1
        do k = 1, cols2
            matrix3(i, k) = 0
            do j = 1, cols1
                matrix3(i, k) = matrix3(i, k) + matrix1(i, j) * matrix2(j, k)
            end do
        end do
    end do
end subroutine
