package com.gym.fit_power.config;


import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractAuthenticationFilterConfigurer;
import org.springframework.security.config.annotation.web.configurers.LogoutConfigurer;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.SecurityFilterChain;

@Configuration
@EnableWebSecurity
public class ConfigurationWeb {
    @Bean
    public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        http
                .csrf(csrf -> csrf.disable()) // Desactiva CSRF
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers("/h2-console/**").permitAll() // Permite acceso a la consola de H2
                        .anyRequest().permitAll() // Permite todas las demás solicitudes
                )
                .headers(headers -> headers
                        .frameOptions(frameOptions -> frameOptions.disable()) // Habilita el uso de frames necesarios para H2
                );

        return http.build();
    }




























   /* @Bean
    public UserDetailsService userDetailsService() {
        InMemoryUserDetailsManager manager = new InMemoryUserDetailsManager();
        //Rol Admin
        manager.createUser(User.withUsername("admin")
                .password("{noop}adminpass")
                .roles("ADMIN")
                .build());

        //Rol client
        manager.createUser(User.withUsername("client")
                .password("{noop}clientpass")
                .roles("CLIENT")
                .build());

        //Rol nutritionist
        manager.createUser(User.withUsername("nutritionist")
                .password("{noop}nutripass")
                .roles("NUTRITIONIST")
                .build());

        //Rol trainer
        manager.createUser(User.withUsername("trainer")
                .password("{noop}trainerpass")
                .roles("TRAINER")
                .build());


        return manager;
    }

    @Bean
    public SecurityFilterChain  securityFilterChain(HttpSecurity http) throws Exception{
        http
                .csrf(csrf -> csrf
                        .ignoringRequestMatchers("/h2-console/**")
                )
                .headers(headers -> headers
                        .contentSecurityPolicy(csp -> csp
                                .policyDirectives("frame-ancestors 'self'")
                        )
                )
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers("/h2-console/**").authenticated()
                        .requestMatchers("/admin/**").permitAll()
                        .requestMatchers("/api/clients/**").hasRole("CLIENT")
                        .requestMatchers("/Nutritionist/**").hasRole("NUTRITIONIST")
                        .requestMatchers("/NutririonPlan/**").hasRole("NUTRITIONIST")
                        .requestMatchers("/api/v1/trainer/**").hasRole("TRAINER")
                        .requestMatchers("/api/gyms/**").hasRole("TRAINER")
                        .requestMatchers("/api/v1/exercise/**").hasRole("TRAINER")
                        .anyRequest().authenticated()
                )
                .formLogin(AbstractAuthenticationFilterConfigurer::permitAll
                )
                .logout(LogoutConfigurer::permitAll
                );

        return http.build();


    }*/

}
