
! Autor: Iván Moya, UPM - GIA 1º

program mincuadrados

    implicit none
    integer, parameter :: unidad = 10, knd = 16
    integer(kind=knd) :: i,n
    logical :: existe
    real(kind=knd) :: x, y, xsum=0, xxsum = 0, ysum=0, xysum = 0, m, b


    inquire(file="datos.dat", exist=existe)

    if(existe .eqv. .false.) then
        do while(1==1)
            open(unidad, file="datos.dat", status="new")
            write(*,"(A,/,A,/,A)") "No estaba el archivo datos.dat, por lo que lo he creado", &
                               "Cierra el programa e introduce datos", &
                               "(Escribe cualquier numero para salir)"
            read(*,*) x
            stop

        end do
    end if

    write(*,*) "Cuantos pares de datos tengo que leer?"
    read(*,*) n
    write(*,"(A,1X,I5,1X,A)") "Confirmo:", n, "datos"

    open(unidad, file="datos.dat", status="old")

    do i=1, n
        read(10, *) x,y
        xsum = xsum + x
        ysum = ysum + y
        xxsum = xxsum + x**2.
        xysum = xysum + x*y

    end do
    close(10)


    m = ( xysum - xsum*(ysum/n) ) / ( xxsum - xsum*(xsum/n) )
    b = ysum/n - m*(xsum/n)

    write(*,"(A, 1X, f6.4,1X,A,f6.4)") "m=",m,"b=",b
    write(*,"(A,1x,f6.4,A,1x,f6.4)") "y =", m, "x +", b

end program mincuadrados
