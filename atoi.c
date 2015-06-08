int my_atoi(char* pointer)
{
    int result = 0;
    char* pointer1;
    int multiplier;
    char sign = 1;

    if(*pointer == '-')
    sign =- 1;  

    pointer1 = pointer;

    while(*pointer != '\0')
    {
        if(*pointer >= '0' && *pointer <= '9')
            multiplier = multiplier * 10;
        pointer = pointer + 1;  
    }

    pointer = pointer1;

    while(*pointer != '\0')
    {       
        if(*pointer >= '0' && *pointer <= '9')
        {
            result = result + ( (*pointer%48)  * multiplier);
            multiplier = multiplier / 10;       
        }
        pointer = pointer+1;
    }

    return (result * sign) / 10;
}
