program secretsanta

    integer*8 :: num_people
    integer*8,parameter :: num_samples = 100000
    integer*8 :: num_people_array(10)
    real*8 :: perfect_pair_array_float(0:size(num_people_array)-1)
    integer*8 :: iter 

    real*8 :: data_array(0:size(num_people_array)-1,0:size(num_people_array)-1)
    
    data_array = 0.0d0

    do iter = 1,size(num_people_array)
        num_people_array(iter) = iter+1
    end do 

    num_people_array(10) = 200
    
    do iter = 1,size(num_people_array)
        num_people = num_people_array(iter)
        write(0,*) 'Working on ',num_people
        call collect_sample_data(num_samples,num_people,perfect_pair_array_float)
        data_array(iter-1,:) = perfect_pair_array_float
    end do  

    write(*,11) num_people_array(:)
    do iter = 1,size(num_people_array)
        write(*,12) iter-1,data_array(:,iter-1)
    end do 

    !10 format('Number of perfect pairs:',I4,1X,'Probability: ',F10.5,'%')
    11 format(6X,200(I10,1X))
    12 format(I5,1X,200(F10.5,1X))

    contains 

    subroutine collect_sample_data(num_samples, num_people,perfect_pair_array_float)
        integer*8 :: num_people, num_samples 
        integer*8 :: array(num_people),assigned_array(num_people)
        real*8,intent(inout) :: perfect_pair_array_float(:)
        integer*8 :: perfect_pair_array(0:size(perfect_pair_array_float)-1)

        integer*8 :: ii,jj

        !init:        
        perfect_pair_array = 0
        assigned_array = 0
        do ii = 1,num_people
            array(ii) = ii
        end do 
        
        !collect data.
        do ii = 1,num_samples
            call distribute(array,assigned_array)
            jj = number_of_perfect_pairs(array,assigned_array)
            perfect_pair_array(jj) = perfect_pair_array(jj) + 1
        end do

        !convert to a probability.
        perfect_pair_array_float = real(perfect_pair_array) / real(num_samples)

    end subroutine


    subroutine remove_element(array, length_of_array,index_to_be_removed)
        integer*8 :: array(:)
        integer*8 ,allocatable :: array_copy(:)
        integer*8 :: length_of_array
        integer*8 :: index_to_be_removed
        allocate(array_copy(size(array)))
        
        array_copy = array 
        array = 0
        array(1:index_to_be_removed) = array_copy(1:index_to_be_removed)
        array(index_to_be_removed:) = array_copy(index_to_be_removed+1:)
        length_of_array = length_of_array-1 
    end subroutine

    subroutine distribute(array_of_people,assigned_array)
        integer*8 :: array_of_people(:),assigned_array(:)
        integer*8 :: array_copy(size(array_of_people))
        integer*8 :: randint,num_people ,num_people_dynamic,ii
        logical :: bad_draw 

        array_copy = array_of_people
        num_people = size(array_of_people)
        num_people_dynamic = num_people
        bad_draw = .true.
        assigned_array = 0
        !print*,'------------'
        do while(bad_draw) 
            !assume this draw is a good one.
            bad_draw = .false.
            !try a draw.

            do ii = 1,num_people

                randint = random_integer(num_people_dynamic)
                !print*,ii
                assigned_array(ii) = array_copy(randint)

                call remove_element(array_copy,num_people_dynamic,randint)
                !print*,'array_copy',array_copy,'assigned_array',assigned_array

                if (ii .eq. assigned_array(ii)) then 
                    !try, try again. 
                    bad_draw = .true.
                    array_copy = array_of_people
                    num_people_dynamic = num_people
                    assigned_array = 0
                    !print*,'reassign'
                    exit
                end if 

            end do 

        end do 



        !print*,assigned_array

    end subroutine

    function random_integer(upper_limit)
        !draws a random integer from 1 to upper_limit.
        integer*8 :: random_integer ,upper_limit
        real*8 :: u 

        call random_number(u)
        random_integer = floor(upper_limit * u) + 1

    end function 

    function number_of_perfect_pairs(array,assigned_array)
        
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


    end function


end program 