#include <cmath>
#include <gtest/gtest.h>

#include <composite_expected.h>


template <typename T>
composite_expected<T, const char*> sqrt( T x )
{
    if ( x >= 0 )
        return composite_expected<T, const char*>::fromVal( std::sqrt( x ) );
    else
        return composite_expected<T, const char*>::fromErr( 0, "Could not take sqr-root of negative number" );
}


TEST( composite_expected, logic )
{
    const auto x = composite_expected<int, int>::fromVal( 100 );
    ASSERT_TRUE( x );
    ASSERT_EQ( x.value(), 100 );

    const auto y = x >> std::bind_front( std::multiplies<>(), -2 );
    static_assert( std::same_as<decltype( y ), const composite_expected<int, int>> );
    ASSERT_TRUE( y );
    ASSERT_EQ( y.value(), -200 );

    auto z = x >> sqrt<int>;
    static_assert( std::same_as<decltype( z ), composite_expected<int, int, const char*>> );
    ASSERT_TRUE( z );
    ASSERT_EQ( z.value(), 10 );

    z = y >> sqrt<int>;
    ASSERT_FALSE( z );
    ASSERT_EQ( z.errorIndex(), 1 );
    ASSERT_EQ( strcmp( std::get<1>( z.error() ), "Could not take sqr-root of negative number" ), 0 );
}


struct MoveCopyCounter
{
    MoveCopyCounter() = default;

    MoveCopyCounter( const MoveCopyCounter& other ):
            copied( other.copied + 1 ),
            moved( other.moved )
    {}

    MoveCopyCounter( MoveCopyCounter&& other ) noexcept:
            copied( other.copied ),
            moved( other.moved + 1 )
    {}

    int copied = 0;
    int moved = 0;
};

auto returnValMove( MoveCopyCounter&& t )
{
    return composite_expected<MoveCopyCounter, MoveCopyCounter>::fromVal( std::move( t ) );
}

auto returnValNoCopy( const MoveCopyCounter& t )
{
    return composite_expected<MoveCopyCounter, MoveCopyCounter>::fromVal( t );
}

auto returnErrCopy( const MoveCopyCounter& t )
{
    return composite_expected<MoveCopyCounter, MoveCopyCounter>::fromErr( 0, t );
}

TEST( composite_expected, move_semantics )
{
    auto x = composite_expected<MoveCopyCounter, MoveCopyCounter>::fromVal();
    ASSERT_EQ( x.value().moved, 0 );

    const auto result = x >> returnValMove >> returnValMove;
    static_assert( std::same_as<decltype(result), const composite_expected<MoveCopyCounter, MoveCopyCounter, MoveCopyCounter, MoveCopyCounter>> );

    ASSERT_TRUE( result );
    ASSERT_EQ( result.value().moved, 6 );
    ASSERT_EQ( result.value().copied, 0 );

    const auto result2 = result >> returnValNoCopy;
    static_assert( std::same_as<decltype(result2), const composite_expected<MoveCopyCounter, MoveCopyCounter, MoveCopyCounter, MoveCopyCounter, MoveCopyCounter>> );

    ASSERT_TRUE( result2 );
    ASSERT_EQ( result2.value().moved, 8 );
    ASSERT_EQ( result2.value().copied, 1 );

    const auto result3 = result >> returnErrCopy >> returnValMove;
    static_assert( std::same_as<decltype(result3), const composite_expected<MoveCopyCounter, MoveCopyCounter, MoveCopyCounter, MoveCopyCounter, MoveCopyCounter, MoveCopyCounter>> );

    ASSERT_TRUE( !result3 );
    std::visit( [] ( const auto& r ) { ASSERT_EQ( r.copied, 2 ); }, result3.error() );
}
