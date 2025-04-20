package AAAConfig;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.web.SecurityFilterChain;

@Configuration
public class SecurityConfig {

    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http
                .authorizeHttpRequests(auth ->
                        auth.requestMatchers("/public/**").permitAll()
                            //.requestMatchers("/admin/**").hasRole("ADMIN")
                            .anyRequest().authenticated())
                .formLogin(formLogin ->
                        formLogin.loginPage("/login").permitAll()
                                 .defaultSuccessUrl("/home", true)
                                 .failureUrl("/login?error=true")
                )
                .logout(logout ->
                        logout.logoutUrl("/logout").permitAll()
                              .logoutSuccessUrl("/login?logout=true")
                //.logoutSuccessHandler(new CustomLogoutSuccessHandler())
                .invalidateHttpSession(true)
                .deleteCookies("JSESSIONID")
                )
                .httpBasic(Customizer.withDefaults())
                .oauth2Login(Customizer.withDefaults())
                .oauth2ResourceServer(
                        oauth2 -> oauth2.jwt(jwt -> jwt.jwkSetUri("https://your-auth-server/.well-known/jwks.json"))
                )
                //.csrf(csrf ->
                //    csrf.ignoringAntMatchers("/h2-console/**") // Disable CSRF for H2 console
                //)
                //.sessionManagement(session ->
                //        session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
                //.addFilterBefore(new CustomAuthenticationFilter(), UsernamePasswordAuthenticationFilter.class)
                //.exceptionHandling(ex ->
                //        ex.authenticationEntryPoint(new HttpStatusEntryPoint(HttpStatus.UNAUTHORIZED)))
                .headers(headers ->
                        headers.frameOptions().sameOrigin()
        );
        return http.build();
    }
}
