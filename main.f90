program secretsanta
    !SecretSanta perfect pair sampling. 
    !Author: Leo Patrick Mulholland https://github.com/LeoMul   
    !with insights from Sean Marshallsay https://github.com/Sean1708

    !This code is maintained at https://github.com/LeoMul/SecretSantaPerfectPairs. 
    use mpi
    logical :: input_exists
    integer*8 :: num_people
    integer*8 :: num_samples, total_num
    integer*8,allocatable:: num_people_array(:)
    real*8 ,allocatable :: perfect_pair_array_float(:),perfect_pair_array_float_master(:)
    real*8 ,allocatable :: data_array(:,:)
    real*8 :: t1,t2
    integer*8 :: iter, ii   
    
    integer*8 :: size_num_people_array
    integer*8 :: max_num_people 

    integer*4,parameter :: seedInt = 123456789
    integer*4,dimension(8) ::seed = seedInt
    
    !mpi stuff:
    integer process_Rank, size_Of_Cluster, ierror


    namelist/SecretSantaInput/ num_samples,num_pops

    call MPI_INIT(ierror)

    call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
    call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)

    call cpu_time(t1)

    seed = seed*(process_Rank+1)
    call random_seed(put = seed)

    !print*,'hello'
    call parseInput 


    size_num_people_array = size(num_people_array)
    max_num_people = maxval(num_people_array)
    allocate(data_array(size_num_people_array,max_num_people))
    data_array = 0.0d0
    num_samples = num_samples / size_Of_Cluster 
    total_num = num_samples * size_Of_Cluster
    
    do ii = 1,size_num_people_array
        num_people = num_people_array(ii)
        allocate(perfect_pair_array_float(0:num_people-1))
        allocate(perfect_pair_array_float_master(0:num_people-1))
        
        call collect_sample_data(num_samples,num_people,perfect_pair_array_float)
       
        call mpi_reduce(perfect_pair_array_float,perfect_pair_array_float_master,int(num_people),mpi_double,mpi_sum,0,mpi_comm_world,ierr)
       
        data_array(ii,1:num_people) = perfect_pair_array_float_master / total_num
       
        deallocate(perfect_pair_array_float)
        deallocate(perfect_pair_array_float_master)

    end do 
    call cpu_time(t2)

    if(process_Rank .eq. 0) then
        write(*,9) total_num
        write(*,10) t2-t1
        write(*,11) num_people_array(:)
        do iter = 1,max_num_people
            write(*,12) iter-1,data_array(:,iter)
        end do
    end if

    call mpi_finalize(ierr)

    !10 format('Number of perfect pairs:',I4,1X,'Probability: ',F10.5,'%')

    !10 format('#',16x,'System size -->')
     9 format('#NumSamples = ',I20)
    10 format('#CPU Time(s) = ',ES10.2)
    11 format('#NumPerfPairs; N = ',1X,200(I10,1X))
    12 format(8x,I5,7X,200(ES20.12,1X))

    contains 

    subroutine collect_sample_data(num_samples, num_people,perfect_pair_array_float)
        integer*8 :: num_people, num_samples 
        integer*8 :: array(num_people),assigned_array(num_people),position_array(num_people)
        real*8,intent(inout) :: perfect_pair_array_float(:)
        integer*8 :: perfect_pair_array(0:size(perfect_pair_array_float)-1)

        integer*8 :: ii,jj



        !init:        

        perfect_pair_array = 0
        assigned_array = 0
        position_array = 0
        do ii = 1,num_people
            array(ii) = ii
        end do 
        write(0,*) num_samples,process_Rank
        !collect data.
        do ii = 1,num_samples
            call distribute(array,assigned_array)
            call numPerfectPairsSean(array,assigned_array,jj)
            perfect_pair_array(jj) = perfect_pair_array(jj) + 1
        end do

        perfect_pair_array_float = real(perfect_pair_array)

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



    subroutine numPerfectPairsSean(array,assigned_array,number_of_perfect_pairs)
        
        !array is an vector of 1,2,3,4,...,size(array)
        !assigned array is the assignment of each element of array.
        !i.e if person1  is assigned 3, the first elment of assigned_array is 3.
        !this function finds the number of perfect pairs
        !i.e the number of times that something that person_i is assigned person_j,
        !and person_j is assigned person_i.

        !credit to Sean Marshallsay for his insights in writing this routine 
        !here the number of pairs is double counted and must be divided by two. 

        integer*8 :: number_of_perfect_pairs 
        integer*8 :: ii,num_people 
        integer*8 :: array(:), assigned_array(:)

        num_people = size(array)
        number_of_perfect_pairs = 0

        !double counts - which im not sure is avoidavle as in principle 
        !pairs can be localised to the last few elements of the array.
        do ii = 1,num_people                
            if ( assigned_array(assigned_array(ii)) .eq. ii) then 
                number_of_perfect_pairs = number_of_perfect_pairs + 1
            end if
        end do 
        number_of_perfect_pairs = number_of_perfect_pairs/2

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
            write(0,*) 'no input is found (this is forced to err so wont be piped if you pipe the above) '
            stop
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


    !The following routines are retired
    subroutine numPerfectPairs(array,assigned_array,number_of_perfect_pairs)
        !this routine has been retired     

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

    subroutine numPerfectPairsWithPosArray(array,assigned_array,number_of_perfect_pairs,position_array)
        
        !retired routine
        !the numPerfectPairsSean routine could also have this kind of addition where the positions are tracked.

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

end program 