#pragma once
#include <concepts>
#include <variant>
#include <exception>

class make_variant_index_error : public std::runtime_error
{
public:
    enum Reason
    {
        BadIndex,
        BadArgs
    };

    explicit make_variant_index_error( Reason reason ):
        std::runtime_error( reason == BadIndex ? "Specified index is greater than number of variant alternatives" : "Specified index is not constructable from specified args" )
    {}
};


namespace details
{

template <typename T>
struct is_variant
{
    static constexpr bool value = false;
};

template <typename ...Ts>
struct is_variant<std::variant<Ts...>>
{
    static constexpr bool value = true;
};

template <typename T>
constexpr bool is_variant_v = is_variant<T>::value;


template <size_t rIndex, typename ...Ts, typename ...Args>
constexpr std::variant<Ts...> make_variant_index_impl_from_args( size_t index, Args&& ...args )
{
    if ( index == rIndex )
    {
        if constexpr ( std::is_constructible_v<std::variant<Ts...>, std::in_place_index_t<rIndex>, Args...> )
            return std::variant<Ts...>( std::in_place_index<rIndex>, std::forward<Args>( args )... );
        else
            throw make_variant_index_error( make_variant_index_error::BadArgs );
    } else if constexpr ( rIndex == sizeof...( Ts ) - 1 )
        throw make_variant_index_error( make_variant_index_error::BadIndex );
    else
        return make_variant_index_impl_from_args<rIndex + 1, Ts...>( index, std::forward<Args>( args )... );
}

template <size_t rIndexNew, size_t rIndexOld, typename ...Ts, typename OtherVariant>
    requires is_variant_v<std::remove_cvref_t<OtherVariant>>
constexpr std::variant<Ts...> make_variant_index_impl_from_other( size_t indexNew, size_t indexOld, OtherVariant&& other )
{
    using OtherVariantNoCvRef = std::remove_cvref_t<OtherVariant>;
    if ( indexNew == rIndexNew )
    {
        if ( indexOld == rIndexOld )
        {
            if constexpr ( std::is_constructible_v<std::variant<Ts...>, std::in_place_index_t<rIndexNew>, std::variant_alternative_t<rIndexOld, OtherVariantNoCvRef>> )
                return std::variant<Ts...>( std::in_place_index<rIndexNew>,
                                            std::get<rIndexOld>( std::forward<OtherVariant>( other ) ) );
            else
                throw make_variant_index_error( make_variant_index_error::BadArgs );
        } else if constexpr ( rIndexOld == std::variant_size_v<OtherVariantNoCvRef> - 1 )
            throw make_variant_index_error( make_variant_index_error::BadIndex );
        else
            return make_variant_index_impl_from_other<rIndexNew, rIndexOld + 1, Ts...>( indexNew, indexOld,
                                                                                        std::forward<OtherVariant>(
                                                                                                other ));
    } else if constexpr ( rIndexNew == sizeof...( Ts ) - 1 )
        throw make_variant_index_error( make_variant_index_error::BadIndex );
    else
        return make_variant_index_impl_from_other<rIndexNew + 1, rIndexOld, Ts...>( indexNew, indexOld,
                                                                                    std::forward<OtherVariant>(
                                                                                            other ));
}

template <typename T>
struct make_variant_index_t;

template <typename ...Ts>
struct make_variant_index_t<std::variant<Ts...>>
{
    template <typename OtherVariant>
        requires is_variant_v<std::remove_cvref_t<OtherVariant>>
    constexpr static std::variant<Ts...> fromOther( size_t indexNew, size_t indexOld, OtherVariant&& other )
    {
        if ( indexNew >= sizeof...( Ts ) || indexOld >= std::variant_size_v<std::remove_cvref_t<OtherVariant>> )
            throw make_variant_index_error( make_variant_index_error::BadIndex );
        else
            return make_variant_index_impl_from_other<0, 0, Ts...>( indexNew, indexOld,
                                                                    std::forward<OtherVariant>( other ));
    }

    template <typename ...Args>
    constexpr static std::variant<Ts...> fromArgs( size_t index, Args&& ...args )
    {
        if ( index >= sizeof...( Ts ))
            throw make_variant_index_error( make_variant_index_error::BadIndex );
        else
            return make_variant_index_impl_from_args<0, Ts...>( index, std::forward<Args>( args )... );
    }
};


template <typename T, typename J>
struct variant_add
{
};

template <typename T, typename J> requires ( !is_variant_v<T> && !is_variant_v<J> )
struct variant_add<T, J>
{
    using type = std::variant<T, J>;
};

template <typename T, typename ...Ts> requires ( !is_variant_v<T> )
struct variant_add<T, std::variant<Ts...>>
{
    using type = std::variant<T, Ts...>;
};

template <typename T, typename ...Ts> requires ( !is_variant_v<T> )
struct variant_add<std::variant<Ts...>, T>
{
    using type = std::variant<Ts..., T>;
};

template <typename ...Ts, typename ...Js>
struct variant_add<std::variant<Ts...>, std::variant<Js...>>
{
    using type = std::variant<Ts..., Js...>;
};


template <typename T>
struct variant_slice_first;

template <typename T, typename ...Ts>
struct variant_slice_first<std::variant<T, Ts...>>
{
    using type = std::variant<Ts...>;
};

template <typename T>
using variant_slice_first_t = typename variant_slice_first<T>::type;


}


template <typename T, typename ...Errs>
class composite_expected
{
public:
    using variant_t = std::variant<T, Errs...>;
private:
    variant_t variant_;

    template <typename Other>
        requires details::is_variant_v<typename Other::variant_t>
    using united_t =
        typename details::variant_add<
            typename details::variant_add<
                std::variant_alternative_t<0, typename Other::variant_t>,
                details::variant_slice_first_t<variant_t>
            >::type,
            details::variant_slice_first_t<typename Other::variant_t>>::type;

    template <typename Ret, typename Other>
        requires details::is_variant_v<std::remove_cvref_t<Other>>
    static auto united( Other&& other )
    {
        return other.index() == 0
                ? from_variant_t<Ret>( details::make_variant_index_t<Ret>::fromOther( 0, 0, std::forward<Other>( other ) ) )
                : from_variant_t<Ret>(
                        details::make_variant_index_t<Ret>::fromOther( sizeof...( Errs ) + other.index(), other.index(), std::forward<Other>( other ) )
                );
    }

public:

    template <typename J>
    struct from_variant;

    template <typename ...Ts>
    struct from_variant<std::variant<Ts...>>
    {
        using type = composite_expected<Ts...>;
    };

    template <typename ...Ts>
    using from_variant_t = typename from_variant<Ts...>::type;

    using value_type = T;

    template <typename ...Args>
    static composite_expected<T, Errs...> fromErr( size_t errIndex, Args&& ...args )
    {
        return composite_expected( details::make_variant_index_t<decltype( variant_ )>::fromArgs( errIndex + 1, std::forward<Args>( args )... ) );
    }

    template <typename ...Args>
    static composite_expected<T, Errs...> fromVal( Args&& ...args )
    {
        return composite_expected( std::in_place_index<0>, std::forward<Args>( args )... );
    }

    template <typename ...Args>
    explicit composite_expected( Args&& ...args ):
        variant_( std::forward<Args>( args )... )
    {}

    template <typename TFwd>
        requires std::same_as<std::remove_cvref<TFwd>, T>
    composite_expected( TFwd&& val ):
        variant_( std::in_place_index<0>, std::forward<TFwd>( val ) )
    {}

    [[nodiscard]] const variant_t& variant() const
    {
        return variant_;
    }

    variant_t&& variant()
    {
        return std::move( variant_ );
    }

    explicit operator bool() const
    {
        return variant_.index() == 0;
    }

    [[nodiscard]] const T& value() const
    {
        return std::get<0>( variant_ );
    }

    T&& value()
    {
        return std::get<0>( std::move( variant_ ) );
    }

    [[nodiscard]] std::variant<Errs...> error() const
    {
        return details::make_variant_index_t<std::variant<Errs...>>::fromOther( variant_.index() - 1, variant_.index(), variant_ );
    }

    std::variant<Errs...> error()
    {
        return details::make_variant_index_t<std::variant<Errs...>>::fromOther( variant_.index() - 1, variant_.index(), std::move( variant_ ) );
    }

    [[nodiscard]] size_t errorIndex() const
    {
        return variant_.index() - 1;
    }

    template <std::invocable<T> Functor>
        requires details::is_variant_v<typename std::invoke_result_t<Functor, T>::variant_t>
    auto then( Functor f )
    {
        using Ret = united_t<std::invoke_result_t<Functor, T>>;
        return *this ? united<Ret>( f( std::move( value())).variant())
                     : from_variant_t<Ret>( details::make_variant_index_t<Ret>::fromOther( variant_.index(), variant_.index(), std::move( variant_ ) ) );
    }

    template <std::invocable<T> Functor>
        requires details::is_variant_v<typename std::invoke_result_t<Functor, T>::variant_t>
    auto then( Functor f ) const
    {
        using Ret = united_t<std::invoke_result_t<Functor, T>>;
        return *this ? united<Ret>( f( value()).variant())
                     : from_variant_t<Ret>( details::make_variant_index_t<Ret>::fromOther( variant_.index(), variant_.index(), variant_ ) );
    }

    template <std::invocable<T> Functor>
    auto then( Functor f )
    {
        using Ret = std::variant<std::invoke_result_t<Functor, T>, Errs...>;
        return *this ? from_variant_t<Ret>::fromVal( f( std::move( value() ) ) )
                     : from_variant_t<Ret>( details::make_variant_index_t<Ret>::fromOther( variant_.index(), variant_.index(), std::move( variant_ ) ) );
    }

    template <std::invocable<T> Functor>
    auto then( Functor f ) const
    {
        using Ret = std::variant<std::invoke_result_t<Functor, T>, Errs...>;
        return *this ? from_variant_t<Ret>::fromVal( f( value() ) )
                     : from_variant_t<Ret>( details::make_variant_index_t<Ret>::fromOther( variant_.index(), variant_.index(), variant_ ) );
    }


    template <std::invocable<T> Functor>
    auto operator >>( Functor f )
    {
        return then( f );
    }

    template <std::invocable<T> Functor>
    auto operator >>( Functor f ) const
    {
        return then( f );
    }
};

