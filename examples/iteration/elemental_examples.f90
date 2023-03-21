module example_elemental_use_cases
contains
    subroutine elemental_addition
        instantiate some_container_tmpl(integer, ...)

        type(container) :: my_container
        integer, allocatable :: my_array(:)

        call my_container%push(1)
        call my_container%push(2)
        call my_container%push(3)
        my_array = [3, 4, 5]
        associate(res => my_container + my_array)
            ! What is res? array or container?
        end associate
    end subroutine

    subroutine foreach
        instantiate some_container_tmpl(integer, ...)

        type(container) :: my_container

        call my_container%push(1)
        ! ...
        foreach thing = my_container ! This kind of thing has become pretty common in other languages
            ! but is there some way that thing's type is inferred?
        end foreach
    end subroutine

    subroutine templated_filter
        template filter_tmpl(container, element, ...)
            type, deferred :: container ! how to say container contains elements?
            type, deferred :: element
        contains
            function filter(stuff, predicate) result(filtered)
                type(container), intent(in) :: stuff
                interface
                  pure function predicate(thing)
                    type(element), intent(in) :: thing
                    logical :: predicate
                  end function
                end interface
                type(container) :: filtered

                foreach thing = stuff
                  if (predicate(thing)) filtered%push(thing)
                end foreach
                ! The modern Fortran way might be
                ! filtered = pack(stuff, predicate(stuff))
            end function
        end template
    end subroutine
end module