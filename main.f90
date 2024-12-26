program secretsanta

    logical :: input_exists
    integer*8 :: num_people
    integer*8 :: num_samples
    integer*8,allocatable:: num_people_array(:)
    real*8 ,allocatable :: perfect_pair_array_float(:)
    real*8 ,allocatable :: data_array(:,:)

    integer*8 :: iter, ii
    
    integer*8 :: size_num_people_array
    integer*8 :: max_num_people 

    namelist/SecretSantaInput/ num_samples,num_pops

    call parseInput 

    size_num_people_array = size(num_people_array)
    max_num_people = maxval(num_people_array)
    allocate(data_array(size_num_people_array,max_num_people))
    data_array = 0.0d0

    do ii = 1,size_num_people_array
        num_people = num_people_array(ii)
        allocate(perfect_pair_array_float(0:num_people-1))
        call collect_sample_data(num_samples,num_people,perfect_pair_array_float)
        data_array(ii,1:num_people) = perfect_pair_array_float
        deallocate(perfect_pair_array_float)
    end do 

    write(*,11) num_people_array(:)
    do iter = 1,max_num_people
        write(*,12) iter-1,data_array(:,iter)
    end do 

    !10 format('Number of perfect pairs:',I4,1X,'Probability: ',F10.5,'%')

    !10 format('#',16x,'System size -->')
    11 format('#NumPerfPairs; N = ',1X,200(I10,1X))
    12 format(8x,I5,7X,200(F10.5,1X))

    contains 

    subroutine main 

    end subroutine

    subroutine collect_sample_data(num_samples, num_people,perfect_pair_array_float)
        integer*8 :: num_people, num_samples 
        integer*8 :: array(num_people),assigned_array(num_people),position_array(num_people)
        !can't assert the size of perfect_pair_array to be num people - 
        real*8,intent(inout) :: perfect_pair_array_float(:)
        integer*8 :: perfect_pair_array(0:size(perfect_pair_array_float)-1)

        integer*8 :: ii,jj!,kk

        integer*4,parameter :: seedInt = 123456789
        integer*4,dimension(8) ::seed = seedInt

        !init:        
        call random_seed(put = seed)

        perfect_pair_array = 0
        assigned_array = 0
        position_array = 0
        do ii = 1,num_people
            array(ii) = ii
        end do 
        
        !collect data.
        do ii = 1,num_samples
            call distribute(array,assigned_array)
            call numPerfectPairs(array,assigned_array,jj)
            !call numPerfectPairsWithPosArray(array,assigned_array,jj,position_array)

            !jj = number_of_perfect_pairs(array,assigned_array)
            !if (jj .eq. 2) then 
            !    print*,'--------------------------------------'
            !    do kk =1,num_people 
            !        write(*,'(3I4)') array(kk),assigned_array(kk),position_array(kk)
            !    end do
            !end if 
            perfect_pair_array(jj) = perfect_pair_array(jj) + 1
        end do

        perfect_pair_array_float = real(perfect_pair_array) / real(num_samples)

    end subroutine


    subroutine remove_element(array, length_of_array,index_to_be_removed)
        !removes an element of an array by shifting the elements above it down
        !empty spaces set to zero - length of array is adjusted by subtracting one.
        integer*8 :: array(:)
        integer*8 ,allocatable :: array_copy(:)
        integer*8 :: length_of_array
        integer*8 :: index_to_be_removed
        allocate(array_copy(size(array)))
        
        array_copy = array 
        array = 0
        array(1:index_to_be_removed-1) = array_copy(1:index_to_be_removed-1)
        if (index_to_be_removed .ne. size(array)) then 
            array(index_to_be_removed:size(array)-1) = array_copy(index_to_be_removed+1:)
        end if 
        length_of_array = length_of_array-1 
    end subroutine

    subroutine distribute(array_of_people,assigned_array)
        
        !distributes an array in a secret santa fashion.
        !if someone draws themself, the whole run is abandoned and the loops returns 
        !to the beginning.

        !This may not be entirely efficient. 

        !variables
        integer*8 :: array_of_people(:),assigned_array(:)
        integer*8 :: array_copy(size(array_of_people))
        integer*8 :: randint,num_people ,num_people_dynamic,ii
        logical :: bad_draw 

        !init
        array_copy = array_of_people
        num_people = size(array_of_people)
        num_people_dynamic = num_people
        bad_draw = .true.
        assigned_array = 0

        do while(bad_draw) 
            !assume this draw is a good one.
            bad_draw = .false.
            !try a draw.

            do ii = 1,num_people
                
                !decide the next draw
                randint = random_integer(num_people_dynamic)
                assigned_array(ii) = array_copy(randint)

                !remove that element from being drawn again. 
                call remove_element(array_copy,num_people_dynamic,randint)

                if (ii .eq. assigned_array(ii)) then 
                    !try, try again, by setting bad_draw to true, 
                    bad_draw = .true.
                    array_copy = array_of_people
                    num_people_dynamic = num_people
                    assigned_array = 0
                    exit 
                    !exits this inner loop, but the outerloop continues as we set bad_draw to true.
                end if 

            end do 

        end do 

    end subroutine

    function random_integer(upper_limit)
        !draws a random integer from 1 to upper_limit.
        integer*8 :: random_integer ,upper_limit
        real*8 :: u 

        call random_number(u)
        random_integer = floor(upper_limit * u) + 1

    end function 

    subroutine numPerfectPairsWithPosArray(array,assigned_array,number_of_perfect_pairs,position_array)
        
        !array is an vector of 1,2,3,4,...,size(array)
        !assigned array is the assignment of each element of array.
        !i.e if person1  is assigned 3, the first elment of assigned_array is 3.
        !this function finds the number of perfect pairs
        !i.e the number of times that something that person_i is assigned person_j,
        !and person_j is assigned person_i.

        !this version of the routine also tracks where the perfect pairs are.
        !was for testing and found to not really slow things down.
        !the bottleneck is porbably in the O(n^2) implementation.

        integer*8 :: number_of_perfect_pairs 
        integer*8 :: ii,jj,num_people 

        integer*8 :: ele1,ele2 
        integer*8 :: ele3,ele4
        integer*8 :: array(:), assigned_array(:), position_array(:)

        position_array = 0
        num_people = size(array)
        number_of_perfect_pairs = 0

        do ii = 1,num_people 
            ele1 = ii  
            ele2 = assigned_array(ii)
            do jj = ii+1,num_people
                ele3 = jj 
                ele4 = assigned_array(jj)

                if ( (ele4 .eq. ele1) .and. (ele2 .eq. ele3)) then 
                    number_of_perfect_pairs = number_of_perfect_pairs + 1
                    position_array(ii) = number_of_perfect_pairs
                    position_array(jj) = number_of_perfect_pairs
                end if 

            end do 
        end do 


    end subroutine

subroutine numPerfectPairs(array,assigned_array,number_of_perfect_pairs)
        
        !array is an vector of 1,2,3,4,...,size(array)
        !assigned array is the assignment of each element of array.
        !i.e if person1  is assigned 3, the first elment of assigned_array is 3.
        !this function finds the number of perfect pairs
        !i.e the number of times that something that person_i is assigned person_j,
        !and person_j is assigned person_i.

        integer*8 :: number_of_perfect_pairs 
        integer*8 :: ii,jj,num_people 

        integer*8 :: ele1,ele2 
        integer*8 :: ele3,ele4
        integer*8 :: array(:), assigned_array(:)

        num_people = size(array)
        number_of_perfect_pairs = 0

        do ii = 1,num_people 
            ele1 = ii  
            ele2 = assigned_array(ii)
            do jj = ii+1,num_people
                ele3 = jj 
                ele4 = assigned_array(jj)

                if ( (ele4 .eq. ele1) .and. (ele2 .eq. ele3)) then 
                    number_of_perfect_pairs = number_of_perfect_pairs + 1
                end if 

            end do 
        end do 


    end subroutine

    subroutine printEmptyInput()
        !print*,'printEmptyInput not implemented yet lpm thu 26.12.24'
        print*,'&SecretSantaInput'
        print*, 'num_samples = 10000'
        print*, 'num_pops = 3'
        print*, '/'
        print*, '2'
        print*, '3'
        print*, '6'
    end subroutine

    subroutine parseInput
        !Parses input.
        inquire(file='ss.inp',exist=input_exists)
        if( .not.(input_exists)) then 
            call printEmptyInput()
            stop 'no input is found '
        end if 
        open(1,file='ss.inp')
        read(1,SecretSantaInput)
        if (num_pops .gt. 0) then 
            allocate(num_people_array(num_pops))
            do ii=1,num_pops
                read(1,*) num_people_array(ii)
            end do 
        else 
            stop 'invalid number of populations'
        end if 
    
        close(1)

    end subroutine

    subroutine write_out()
        
    end subroutine  

end program 